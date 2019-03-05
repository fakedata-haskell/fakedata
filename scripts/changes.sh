#!/usr/bin/env bash

cd ../faker/lib/locales/
ls *yml ./en/*yml | xargs sha256sum
