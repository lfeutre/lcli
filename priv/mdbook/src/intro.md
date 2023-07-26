# Introduction

lcli (pronounced *"Elk-ly"*) is a semi-opinionated library for creating command line tools and supporting options (flags) and arguments (non-flag parameters) parsing. Regular options parsing is done using the Erlang [getopt](https://github.com/oubiwann/getopt-erl) community library. lcli manages the handling of commands and nested subcommands.

This library attempts to provide CLI-tool-building capabilities to LFE developers in such a way that it feels LFE-idiomatic. Two excellet libraries have served as inspiration, from two different programming languages: Rust's [clap](https://docs.rs/clap/latest/clap/) and Go's [kong](https://github.com/alecthomas/kong) (with the former having a bigger influence).
