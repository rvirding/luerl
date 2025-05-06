# Luerl

*An implementation of Lua in Erlang*

[![Build Status][gh-actions-badge]][gh-actions]
[![Luerl Versions][luerl-badge]][luerl]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

**Alert**: The migration from Lua 5.2 to 5.3 is very much Work-In-Progress. Please test it but there are as yet no guarantees.

## About

Luerl is an implementation of standard Lua 5.3 written in Erlang/OTP.

Lua is a powerful, efficient, lightweight, embeddable scripting language common in games, IoT devices, AI bots, machine learning and scientific computing research.

It supports procedural, object-oriented, functional, data-driven, reactive, organizational programming and data description.

Being an extension language, Lua has no notion of a "main" program: it works as a library embedded in a host simple called the embedding program. The host program can invoke functions to execute a piece of Lua code, can write and read Lua variables, and can call Erlang functions by Lua code.

Through the use of Erlang functions, Luerl can be augmented to cope with a wide range of different domains, creating a customized language sharing a syntactical framework.

Luerl is implemented as a library, written in clean Erlang/OTP. For more information, read the [documentation](https://github.com/rvirding/luerl/wiki) and follow the [get started](https://github.com/rvirding/luerl/wiki/0.2-Getting-started) tutorial. You may also browse the [examples](https://github.com/rvirding/luerl/tree/develop/examples).

## Join the Community

[Luerl on Slack](https://luerl.slack.com), join by requesting an invite [here](https://erlef.org/slack-invite/luerl)

[Luerl Forum - Erlang Forums](https://erlangforums.com/luerl)

Luerl embraces both [#Erlang](https://twitter.com/hashtag/erlang?src=hash) and [#LuaLang](https://twitter.com/hashtag/lualang?src=hash) communities and ecosystems.

[//]: ---Named-Links---

[logo]: priv/images/logo.png
[logo-large]: priv/images/logo-large.png
[gh-actions-badge]: https://github.com/rvirding/luerl/workflows/Test/badge.svg
[gh-actions]: https://github.com/rvirding/luerl/actions
[luerl]: https://github.com/rvirding/luerl
[luerl-badge]: https://img.shields.io/badge/luerl-1.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-24%20to%2026-blue.svg
[versions]: https://github.com/rvirding/luerl/blob/master/.github/workflows/ci.yml
[github-tag]: https://github.com/rvirding/luerl/tags
[github-tag-badge]: https://img.shields.io/github/tag/rvirding/luerl.svg
