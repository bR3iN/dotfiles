#!/usr/bin/python3

from dataclasses import dataclass, field
import logging
from os import getenv, makedirs, remove, symlink
from os.path import abspath, basename, dirname, exists, isabs, isdir, join
from posixpath import islink
from shutil import rmtree
import subprocess
from sys import argv
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
    Purple = Colorizer("\x1b[0;35m")


class ColorFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord):
        level_color = {
            logging.DEBUG: Color.Cyan,
            logging.INFO: Color.Green,
            logging.WARNING: Color.Yellow,
            logging.ERROR: Color.Red,
            logging.CRITICAL: Color.Red.bold,
        }[record.levelno]

        fmt = "{} %(message)s".format(level_color("%(levelname)s"))
        return logging.Formatter(fmt).format(record)


@dataclass
class Target:
    install: List[str] = field(default_factory=list)
    system_install: List[str] = field(default_factory=list)
    cmds: List[Union[str, List[str]]] = field(default_factory=list)
    links: Dict[str, str] = field(default_factory=dict)


@dataclass
class Config:
    targets: Dict[str, Target] = field(default_factory=dict)

    @staticmethod
    def from_file(path: str) -> 'Config':
        logging.info(f"Reading config from {path}")
        with open(path, 'rb') as fh:
            toml = tomllib.load(fh)

        targets = {name: Target(**opts)
            for name, opts in toml.items()}

        return Config(targets=targets)


@dataclass
class Env:
    workdir: str
    targetdir: str
    user_bin: str
    system_bin: str
    shell: str

    @staticmethod
    def default() -> 'Env':
        home = getenv('HOME')
        assert home is not None, 'HOME is not set'
        return Env(workdir=dirname(abspath(__file__)),
                   targetdir=home,
                   user_bin=join(home, '.local', 'bin'),
                   system_bin='/usr/local/bin',
                   shell='bash')


def _ensure_absolute(path: str, basedir: str):
    return path if isabs(path) else abspath(join(basedir, path))


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
            self._system_install(join(self._env.workdir, path))

        for path in target.install:
            self._user_install(join(self._env.workdir, path))

        for cmd in target.cmds:
            self._exec(cmd)


    def _user_install(self, path: str):
        if isdir(path):
            self._exec(['make', 'install'], cwd=path)
        else:
            dst = join(self._env.user_bin, basename(path))
            self._create_link(path, dst)


    def _system_install(self, path: str):
        if isdir(path):
            self._exec(['sudo', 'make', 'install'], cwd=path)
        else:
            dst = join(self._env.system_bin, basename(path))
            self._exec(['sudo', 'cp', path, dst])


    def _create_link(self, src: str, dst: str):
        if self._ensure_good_target(dst):
            logging.info(f'Linking {Color.Yellow(src)} to {Color.Yellow(dst)}')
            symlink(src, dst)


    def _ensure_good_target(self, path: str) -> bool:
        if islink(path):
            logging.debug(f'Removing existing link {path}')
            remove(path)
        elif exists(path):
            if self._have_user_confirm(f'{path} already exists. Delete it?'):
                logging.info(f'Deleting {path}')
                rmtree(path)
            else:
                return False
        elif not exists(dir := dirname(path)):
            logging.debug(f'Creating {dir}')
            makedirs(dir)
        return True


    @staticmethod
    def _fmt_cmd(cmd: List[str]) -> str:
        sep_color = Color.Normal.dim
        return sep_color('[') + sep_color(", ").join(map(Color.Purple.bold, cmd)) + sep_color(']')


    def _exec(self, cmd: Union[str, List[str]], cwd: Optional[str] = None):
        if isinstance(cmd, str):
            cmd = [self._env.shell, '-c', cmd]
        if cwd is None:
            cwd = self._env.workdir

        logging.info(f'Running command {self._fmt_cmd(cmd)} inside {Color.Blue.bold(cwd)}')
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


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    logging.getLogger().handlers[0].setFormatter(ColorFormatter())

    env = Env.default()
    config = join(env.workdir, 'dotfiles.toml')
    runner = Runner(config=Config.from_file(config), env=env)

    for target in argv[1:]:
        runner.run(target)
