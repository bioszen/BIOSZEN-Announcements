# BIOSZEN Announcements

## Purpose
This repository serves remote announcements for BIOSZEN. The app downloads static DCF files over HTTP (raw GitHub) to decide what to show.

## Stable URLs
The app only needs a BASE_RAW. From that base it downloads:
- /announcements/index.dcf
- /announcements/items/<latest>.dcf

## How to publish a new announcement
1) Create a new file at announcements/items/<new_id>.dcf
2) Inside the file, set id: <new_id>
3) Edit announcements/index.dcf and set latest: <new_id>
4) Commit + push to main

## Rules
- Do not rename the announcements/ folder
- Do not rename the items/ folder
- Do not delete old items
- Do not change the default branch (keep main) or, if it changes, update BASE_RAW in the app

## Raw URL examples
- index: https://raw.githubusercontent.com/OWNER/bioszen-announcements/main/announcements/index.dcf
- item: https://raw.githubusercontent.com/OWNER/bioszen-announcements/main/announcements/items/2026-01-02-welcome.dcf

## App configuration
- BASE_RAW = https://raw.githubusercontent.com/OWNER/bioszen-announcements/main
