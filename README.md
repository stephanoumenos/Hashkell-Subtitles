# Hashkell-Subtitles

Command line utility to automatically download subtitles for your files using their calculated hash.

(This is still being tested)

## Usage

First go to [Open Subtitles](https://www.opensubtitles.org) and find out your language code. You can do that by just selecting your language, pressing Search and it's going to be at the end of the url.

For example, for Brazilian Portuguese, that link would be:

https://www.opensubtitles.org/en/search/sublanguageid-pob

Therefore its code is pob.


Then just run the program in whichever file you want (multiple files are accepted):

```
hashkell-subtitles -l pob foo.mkv bar.mkv RickyAndMorty/*.mkv
```

It is going to automatically decompress the downloaded file and save it with the same name as the video file to make things easier.

## Building

Make sure you've got stack installed. Clone this repo, cd there and just type "stack install".
Maybe I'll provide a compiled package or a docker image one day.

## Future features

- [] Add parallelism
- [] Support a config file