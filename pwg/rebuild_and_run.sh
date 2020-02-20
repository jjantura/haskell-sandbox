#!/bin/bash
stack build && stack exec pwg-exe && ./convert.sh
