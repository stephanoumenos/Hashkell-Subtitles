# Hashkell-Subtitles

Command line utility to automatically download subtitles for your files using their calculated hash.

(This is still being tested, but should be _functional_ for most files)

## Usage

First go to [Open Subtitles](https://www.opensubtitles.org) and find out your language code. You can do that by just selecting your language, pressing Search and it's going to be at the end of the url.

For example, for Brazilian Portuguese, that link would be:

https://www.opensubtitles.org/en/search/sublanguageid-pob

Therefore its code is pob.


Then just run the program in whichever file you want (multiple files are accepted):

```
hashkell-subtitles -l pob foo.mkv bar.mkv RickyAndMorty/*.mkv
```

It is going to automatically decompress the downloaded file and save it with the same name as the video file to make things easier. (Beware: it will overwrite existing .srt files)

It will also check if the file has a valid video extension (e.g. mp4)

The program also defaults to a async behavior (i.e. it will try to download all subtitles at once). You can use the -s (or --sequential) parameter to avoid that if you want.

## Flags

All of the flags below are optional

| short | long | use |
|:-----:|:----:|:---:|
| -s    | --sequential    | Don't use async, i.e. download subtitles sequentially |
| -l    | --language-code | Language code to download subtitles for               |
| -r    | --recursive     | Search files recursively if directories are provided  |

## Config file

You can also set your default language in a config file so you don't have to pass the -l argument every time.

To do that, just create a text file in ```~/.config/hashkell-subtitles/config``` and write your language code like this:

```
lang = SUBSTITUTE_YOUR_LANGUAGE_CODE_HERE
```

## Building

Make sure you've got stack installed. Clone this repo, cd there and just type "stack install".
Maybe I'll provide a compiled package or a docker image one day.

## Future features

- [X] Add parallelism
- [X] Support a config file

## Bugs

- There's currently a bug in the Open Subtitles REST API making it don't correctly read hashes starting with one or more zeros. There's nothing much I can do about that at the moment except reporting it (which I already have), so this program shouldn't work in about ~ 10% of the files right now. :(
