#!/usr/bin/python3

import argparse
from urllib.parse import urlparse
from urllib.request import urlopen
from dataclasses import dataclass, field
import json
import logging
from os import getenv, makedirs, remove, symlink
from os.path import abspath, basename, dirname, exists, isabs, isdir, join
from posixpath import islink
from shutil import rmtree
import subprocess
import tomllib
from typing import Dict, List, Optional, Union


class Colorizer(str):
    def __call__(self, text: str) -> str:
        return self + text + "\x1b[0m"

    @property
    def bold(self) -> 'Colorizer':
        return Colorizer(self + "\x1b[1m")

    @property
    def dim(self) -> 'Colorizer':
        return Colorizer(self + "\x1b[2m")


class Color:
    Normal = Colorizer("\x1b[0m")
    Red = Colorizer("\x1b[0;31m")
    Yellow = Colorizer("\x1b[0;33m")
    Green = Colorizer("\x1b[0;32m")
    Cyan = Colorizer("\x1b[0;36m")
    Blue = Colorizer("\x1b[0;34m")
    Magenta = Colorizer("\x1b[0;35m")


class ColorFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord):
        level_color = {
            logging.DEBUG: Color.Cyan,
            logging.INFO: Color.Green,
            logging.WARNING: Color.Yellow,
            logging.ERROR: Color.Red,
            logging.CRITICAL: Color.Red.bold,
        }[record.levelno]

        fmt = f"{level_color('%(levelname)s')} {record.name} %(message)s"
        return logging.Formatter(fmt).format(record)


@dataclass
class Target:
    install: List[str] = field(default_factory=list)
    system_install: List[str] = field(default_factory=list)
    cmds: List[Union[str, List[str]]] = field(default_factory=list)
    links: Dict[str, str] = field(default_factory=dict)


@dataclass
class Options:
    workdir: str = "."


@dataclass
class Config:
    options: Options
    targets: Dict[str, Target] = field(default_factory=dict)

    @staticmethod
    def from_file(path: str) -> 'Config':
        with open(path, 'rb') as fh:
            toml = tomllib.load(fh)

        targets = {name: Target(**opts)
            for name, opts in toml.pop('target').items()}

        options = Options(**toml)

        return Config(options=options, targets=targets)


@dataclass
class Env:
    workdir: str
    targetdir: str = getenv('HOME')  # type: ignore
    user_bin: str = join(getenv('HOME'), '.local', 'bin')  # type: ignore
    system_bin: str = "/usr/local/bin"
    shell: str = "/usr/bin/bash"


def _ensure_absolute(path: str, basedir: str):
    return path if isabs(path) else abspath(join(basedir, path))


def is_url(url: str) -> bool:
    parsed = urlparse(url)
    return bool(parsed.scheme) and bool(parsed.netloc)


class UserAbort(BaseException):
    pass


class Runner:
    def __init__(self, env: Env, config: Config):
        self._env = env
        self._config = config


    def run(self, name: str):
        target = self._config.targets[name]

        for dst, src in target.links.items():
            self._create_link(src=join(self._env.workdir, src),
                              dst=_ensure_absolute(dst, self._env.targetdir))

        for path in target.system_install:
            self._system_install(path)

        for path_or_url in target.install:
            self._user_install(path_or_url)

        for cmd in target.cmds:
            self._exec(cmd)


    def list_targets(self, verbose: bool):
        if not verbose:
            print(Color.Green.bold("Targets:"))
            print("========")
            for target in self._config.targets.keys():
                    print(Color.Yellow(target))
        else:
            print(json.dumps(
                {target: opts.__dict__ for target, opts in self._config.targets.items()},
                indent=4))



    def _user_install(self, path: str):
        if is_url(path):
            dst = join(self._env.user_bin, basename(path))
            self._download_to(path, dst)
            self._exec(['chmod', '+x', dst])
        else:
            path = join(self._env.workdir, path)
            if isdir(path):
                self._exec(['make', 'install'], cwd=path)
            else:
                dst = join(self._env.user_bin, basename(path))
                self._create_link(path, dst)
                self._exec(['chmod', '+x', dst])


    def _system_install(self, path: str):
        path = join(self._env.workdir, path)
        if isdir(path):
            self._exec(['sudo', 'make', 'install'], cwd=path)
        else:
            dst = join(self._env.system_bin, basename(path))
            self._exec(['chmod', '+x', path])
            self._exec(['sudo', 'cp', path, dst])


    def _download_to(self, url: str, out_path: str, expected_status=200):
        self._ensure_good_target(out_path)
        logging.info(f'Downloading {url} to {out_path}')
        with urlopen(url) as resp:
            assert resp.status == expected_status, f'Bad HTTP status {resp.status}'
            with open(out_path, 'wb') as fh:
                fh.write(resp.read())


    def _create_link(self, src: str, dst: str):
        assert exists(src), f'{src} does not exist'
        if self._ensure_good_target(dst):
            # Also removes superflous path components like "/../"
            src = abspath(src)
            logging.info(f'Linking {Color.Yellow(src)} to {Color.Yellow(dst)}')
            symlink(src, dst)


    def _ensure_good_target(self, path: str) -> bool:
        if islink(path):
            logging.debug(f'Removing existing link {path}')
            remove(path)
        elif exists(path):
            if self._have_user_confirm(f'{path} already exists. Delete it?'):
                logging.info(f'Deleting {path}')
                if isdir(path):
                    rmtree(path)
                else:
                    remove(path)
            else:
                return False
        elif not exists(dir := dirname(path)):
            logging.debug(f'Creating {dir}')
            makedirs(dir)
        return True


    @staticmethod
    def _fmt_cmd(cmd: List[str]) -> str:
        sep_color = Color.Normal.dim
        return sep_color('[') + sep_color(", ").join(map(Color.Blue.bold, cmd)) + sep_color(']')


    def _exec(self, cmd: Union[str, List[str]], cwd: Optional[str] = None):
        if isinstance(cmd, str):
            cmd = [self._env.shell, '-c', cmd]

        logging.info('Running command {}{}'
                     .format(self._fmt_cmd(cmd), f' inside {Color.Blue.bold(cwd)}' if cwd is not None else ""))

        if cwd is None:
            cwd = self._env.workdir

        ec = subprocess.run(cmd, cwd=cwd).returncode

        if ec != 0:
            logging.error(f'Command {self._fmt_cmd(cmd)} failed with exit code {Color.Red.bold(str(ec))}')


    def _have_user_confirm(self, prompt: str) -> bool:
        while True:
            match input(f'{prompt} ([y]es/[N]o/[a]bort)').lower():
                case 'y':
                    return True
                case 'n':
                    return False
                case 'a':
                    raise UserAbort()
                case _:
                    print("Please answer one of 'y', 'n', or 'a'")


@dataclass
class CLI:
    targets: List[str]
    verbose: bool
    list: bool
    file: Optional[str] = None

    @staticmethod
    def from_args() -> 'CLI':
        parser = argparse.ArgumentParser()
        parser.add_argument("-v", "--verbose", action="store_true",
                            help="Enable verbose output")
        parser.add_argument("-l", "--list", action="store_true",
                            help="List targets and exit")
        parser.add_argument("-f", "--file", type=str,
                            help="Config file (toml) to use")
        parser.add_argument("targets", nargs="*",
                            help="Config targets to install")
        return CLI(**vars(parser.parse_args()))


if __name__ == "__main__":
    cli = CLI.from_args()

    logging.basicConfig(level=logging.DEBUG if cli.verbose else logging.INFO)
    logging.getLogger().name = Color.Normal.dim("None")
    logging.getLogger().handlers[0].setFormatter(ColorFormatter())

    config_path = abspath(cli.file or join(dirname(abspath(__file__)), 'dotfiles.toml'))
    logging.info(f"Using config at {Color.Yellow(config_path)}")
    config = Config.from_file(config_path)

    workdir = _ensure_absolute(config.options.workdir, dirname(config_path))
    logging.info(f'Using workdir {Color.Yellow(workdir)}')
    env = Env(workdir=workdir)

    runner = Runner(config=config, env=env)

    if cli.list:
        runner.list_targets(cli.verbose)
    elif len(cli.targets) == 0:
        logging.error('No target specified')
        exit(1)
    else:
        for target in cli.targets:
            logging.getLogger().name = Color.Magenta.bold(target)
            runner.run(target)
