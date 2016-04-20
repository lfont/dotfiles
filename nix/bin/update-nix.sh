#!/usr/bin/env bash

nix-channel --update
nix-env -u '*'
