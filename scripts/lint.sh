#!/bin/bash

cd idream

exec ../.venv/bin/flake8 --ignore E501,W503,F403,F405 --exclude __pycache__ .
