---
title: NixOS config template considerations
---


Here is the summary of responses on both threads

[Discource](https://discourse.nixos.org/t/any-default-config-for-reproducible-configurations/5268)
and
[Reddit](https://www.reddit.com/r/NixOS/comments/eihw31/any_default_config_for_reproducible_configurations/)

## Pinning the base nixpkgs versions

### Use niv

* Pros
  * Generates boilerplate code to working without the `nixpkgs`. Can
    be used to pin the base `nixpkgs`.
  * Can update itself
  * Can update all external dependencies in one command.
  * Works like `nix-prefetch-*` command, so the sources are
    distributed via the nix store, which is more efficient and fast
    than `git submodule` method
  * No need for the `niv` binary on evaluation (and build) stage. The
    generated nix code is independent from the `niv`.
  * Written in Haskell. Contains less bugs.

* Cons
  * `niv` is not in the `nixpkgs`
  * Works with only GitHub dependencies. Raw `git` support marked as
    experimental.
  * New software. May be unstable.
  * Written in Haskell. Will pull some dependencies and have long
    compilation time.

### Use flakes

* Pros
  * Modularity and coposability of the flakes

* Cons
  * Very experimental and raw for now. May require patched `nix`
  * Requires all parts of your configuration to be flakes

### Use git submodule to pin specific commit of nixpkgs

* Pros
  * No need to use nix expressions to manage pinned version

* Cons
  * Conflicts resolution in git submodules is a mess
  * Slow submodule update. Especially when last update was long time
    ago.

## Pinning secondary nixpgs versions/overlays/source trees/other external repositories

### Use niv

Same as previous

### Use flakes

Same as previous

### fetchFromGitHub

* Pros
  * Simple enough

* Cons
  * Needs some directory with .json files results of `nix-prefetch-github`
  * Mess in the code like

      ```nix
      nixpkgsUnstablePath = self.fetchFromGitHub (
        lib.importJSON ./nixpkgs-unstable.json
      ) ;
      nixpkgsUnstable = import nixpkgsUnstablePath {
        config = self.config;
      } ;

      ```

  * Manual updating each dependency with commands like

      ```shell
      git log -n1 --format="%H" channels/nixos-unstable
      nix-prefetch-github NixOS nixpkgs-channels --rev e4134747f5666bcab8680aff67fa3b63384f9a0f > nixpkgs-unstable.json
      ```

  not very convenient

## Multiple machines configuration

### Use imports section

Create some top-level files like `host1.nix`, `host2.nix`
with `imports` section, like

```nix
  imports =
    [ ./hostX/hardware-configuration.nix
      ./some-configuration.nix
      ./user-packages
    ];
```

Create symlink on each machine pointing to specific `hostX.nix` file.

Update the system with `nixos-rebuild switch`

* Pros
  * Simple and inspectable.

* Cons
  * Need to support manual imports
  * Possible code mess

#### Improvement

May be improved by structured files tree. Like

```text
.
├── configuration.nix -> hosts/host1/default.nix
├── hosts
│   ├── host1
│   │   ├── default.nix
│   │   ├── hardware-configuration.nix
│   │   └── users
│   │       └── user1.nix
│   └── host2
│       ├── default.nix
│       ├── hardware-configuration.nix
│       └── overlays
│           └── default.nix
├── overlays
│   ├── default.nix
│   ├── overlay1
│   │   └── default.nix
│   └── overlay2
│       └── default.nix
├── shared
│   ├── default.nix
│   └── package-sets.nix
└── users
    ├── default.nix
    ├── user1.nix
    └── user2.nix
```

So the `imports` section of each `hosts/hostX/default.nix` may look like

``` nix
  imports =
    [ ./hardware-configuration.nix
      ./users
      ../../overlays
      ../../users
      ../../shared
    ];
```

### Use modules

To be honest, I did not find any pros comparing to previous one.

## Managing secrets
### git-crypt

* Pros
  * Claimed to be secure
* Cons
  * May conflict with some ways of editing the config files

### Manual copying the secrets to each machine

* Cons
  * No way for centralized secret management

## Editing the config files

The question is how to edit files in the `/etc/nixos`. The files are
managed with `git` but you still need to push it to the github (and so
have keys loaded in your ssh agent or enter password every time). Also
editing files as root is not very convenient and secure in case of GUI
editors.

### Edit directly as root user with vim/nano

* Pros
  * Simple

* Cons
  * The root user usually dont have convenient dev tools in it's
    environment (and must do not)

### Have a copy of the repository in a home of user

First you have some copy of your config somewhere in
`/home/user/nixos-config` in which you make some changes, test them
with `nixos-rebuild build` and commit.

Then, in the `/etc/nixos` repository have setting like

```shell
git remote add local file:///home/user/nixos-config
```

And fetch all changes into the root repository with `git pull` from
the user's copy

then `nixos-rebuild switch` as root user

* Pros
  * May store secrets in unencrypted form in only the `/etc/nixos`
  * No posibility to change system configuration as unprivileged user

* Cons
  * Excess copy-paste actions (which may be automated with scripts)
  * Excess space (and time to pull) for the `nixpkgs` submodule (if
    you use this way of pinning)

### Symlink to folder in a user's home
## Managing the home folder

Should be optional to use home-manager
