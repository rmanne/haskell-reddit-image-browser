# hs-reddit-image-browser

Reddit image browse for haskell. Supports only link posts (eg: r/pics). Supports user saved list as well as subreddits.

Downloader is based on aria2c, so the user is expected to have an aria2c instance already running before starting this app.

Originally, this was implemented using a gstreamer plugin but this was quite buggy and I have since switched it to use SDL and FFmpeg directly. The UI is heavily inspired by the VPlay demo available in https://hackage.haskell.org/package/ffmpeg-light-0.13.0 but makes a number of improvements to reduce memory usage which caused crashes in the past.

Currently supports a variety of image URLs, but currently has limited support for imgur galleries. This also supports NSFW gfycat images, which are redirected to redgifs.

A config file is also expected at `~/.config/hs-reddit-image-browser/config.yaml` (assuming your XDG config directory is `~/.config`). Format:

    userAgent: <some user agent>
    clientId: <reddit oauth client ID (optional if no special args)>
    secret: <reddit oauth secret (optional if no special args)>
    path: /home/nobody/media/reddits/
    aria2Secret: asdf
    specialArgs:
      rmanne:
        route:
          - user
          - rmanne
          - saved
        refreshToken: <the corresponding refresh token>

Example usage:

    rib rmanne
    rib pics
