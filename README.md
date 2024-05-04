Gamelle is a tiny 2D game library/engine, perfect for gamejams or as an excuse to learn OCaml!

- **Automatic assets loading** for images, sounds and fonts.
- **Hot code reload** on every file change for a quick feedback loop.
- **Export to a single HTML file** to share with your friends, including all assets and source code.
- **Immediate mode GUI** to quickly put together menus and widgets.
- **Rigid physics**, collisions detection, animations, ...

We hope the library will be easy to learn and fun to use, as there should be few requirements to be productive: [**Online Documentation**]()

```ocaml
open Gamelle

let () =
  run 0.0 @@ fun ~io x ->
  Text.draw ~io "Hello Gamelle!" ~at:(Point.v x x);
  x +. 1.0
```

![Screenshots of sokoban, snake, physics, gui demo, tic-tac-toe, pong](https://art-w.github.io/gamelle/screenshots.png)

***This is a work in progress!** Only the most adventurous should try it at this point. While features should work, their implementation is super raw, not battle tested, with terrible code as we are learning along the way... so bugs, missing functionalities, api changes, etc etc are expected! Feel free to file an issue or open a PR if you want to help :)*

## Getting started

Installation:

```bash
$ opam install https://github.com/art-w/gamelle
```

Installing the library will also install a `gamelle` executable, which can be used to create a fresh new game project:

```bash
$ gamelle init my-super-game
$ cd my-super-game
my-super-game/ $ make   # launch the game!
```

While you can use the `gamelle` library by setting up the dune files yourself, the starter project enables all of the feature out of the box:

```bash
$ tree
├── assets/    # put your images and sounds in this folder
├── dune
├── dune-project
├── Makefile
├── my_super_game.template.html    # customize this html for a release
└── src/
    ├── dune
    └── my_super_game.ml    # sources of your game
```

The sources of your game will be in the `src/` folder, with the entry point `my_super_game.ml`. Note that running the `make` command will run your game with hot code reload enabled: Every change to the source code is immediately reflected in the running game!

As a minimal example, the following "game" allows the player to move left and right:

```ocaml
open Gamelle

(* A type to represent your game state, here the player position *)
type state = { player_position : Point.t }

let initial_state = { player_position = Point.v 100.0 100.0 }

let () =
  (* Run your game! *)
  Gamelle.run initial_state @@ fun ~io { player_position } ->

  (* Called on every frame to react to the latest player inputs *)
  let dir =
    if Input.is_pressed ~io `arrow_right then Vec.v 1.0 0.0
    else if Input.is_pressed ~io `arrow_left then Vec.v (-1.0) 0.0
    else Vec.zero
  in
  let player_position = Vec.(player_position + dir) in

  (* Draw the player character, using the bitmap [assets/player.png] *)
  draw ~io ~at:player_position Assets.player;

  (* Return the new state for the next frame, with the updated player position *)
  { player_position }
```

## Loading assets

Any file you put in the `assets/` folder will automatically be made available in the `Assets` module:

- `assets/player.png` becomes `Assets.player` of type `Bitmap.t` (an image)
- `assets/boing.mp3` becomes `Assets.boing` of type `Sound.t` (a sound or music)
- `assets/comic-sans.ttf` becomes `Assets.comic_sans` of type `Font.t` (a typeface used to draw text)

If you are looking for 2d images that you can use in your game, we recommend the [Kenney game assets](https://kenney.nl/assets/category:2D) collection!

## Release to HTML

The `make html` command will produce a single HTML file with all the code and assets included. It's intended to ease the distribution of your game as you can share this file with your friends or upload it to a static host (for example to your github pages). Send us your games and we'll list them below!

You can customize the html/css surrounding your game by editing the `mygame.template.html` file. After running `dune build --profile=release mygame.html`, you should find the resulting html file at `_build/default/mygame.html`.
