#!/usr/bin/python3

import subprocess
from shutil import rmtree
from dataclasses import dataclass, field
from posixpath import islink
from sys import argv
import tomllib
from typing import Dict, List, NoReturn, Optional, Union
from os.path import abspath, basename, dirname, exists, isabs, isdir, join
from os import getenv, remove, symlink

@dataclass
class Target:
    install: List[str] = field(default_factory=list)
    system_install: List[str] = field(default_factory=list)
    links: Dict[str, str] = field(default_factory=dict)

@dataclass
class Machine:
    targets: List[str] = field(default_factory=list)

@dataclass
class Options:
    pass

@dataclass
class Config:
    options: Options
    machines: Dict[str, Machine] = field(default_factory=dict)
    targets: Dict[str, Target] = field(default_factory=dict)

    @staticmethod
    def from_file(path: str) -> 'Config':
        with open(path, 'rb') as fh:
            toml = tomllib.load(fh)

        targets = {name: Target(**opts)
            for name, opts in toml.pop('target', {}).items()}
        machines = {name: Machine(**opts)
            for name, opts in toml.pop('machine', {}).items()}
        options = Options(**toml)

        return Config(options=options, targets=targets, machines=machines)


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

        for src, dst in target.links.items():
            self._create_link(src=join(self._env.workdir, dst),
                              dst=_ensure_absolute(src, self._env.targetdir))

        for path in target.system_install:
            self._system_install(join(self._env.workdir, path))

        for path in target.install:
            self._user_install(join(self._env.workdir, path))


    def _user_install(self, path: str):
        if isdir(path):
            self._exec(['make'], cwd=path)
        else:
            dst = join(self._env.user_bin, basename(path))
            self._create_link(path, dst)


    def _system_install(self, path: str):
        if isdir(path):
            self._exec(['sudo', 'make'], cwd=path)
        else:
            dst = join(self._env.system_bin, basename(path))
            self._exec(['sudo', 'cp', path, dst])


    def _create_link(self, src: str, dst: str):
        if self._ensure_non_existent(dst):
            symlink(src, dst)


    def _ensure_non_existent(self, path: str) -> bool:
        if exists(path):
            if islink(path):
                remove(path)
            else:
                if self._have_user_confirm(f'{path} already exists. Delete it?'):
                    rmtree(path)
                else:
                    return False
        return True


    def _exec(self, cmd: Union[str, List[str]], cwd: Optional[str] = None):
        if isinstance(cmd, str):
            cmd = [self._env.shell, '-c', cmd]
        if cwd is None:
            cwd = self._env.workdir
        subprocess.run(cmd)


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


    def setup_machine(self, name: str):
        machine = self._config.machines[name]


if __name__ == "__main__":
    runner = Runner(config=Config.from_file(CONFIG),
                    env=Env.default())

    for target in argv[1:]:
        runner.run(target)
