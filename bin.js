#!/usr/bin/env node

'use strict';

const commandName = process.argv[2];
const fs = require('fs');
const path = require('path');
const cp = require('child_process');

if (!commandName) {
  console.error(`  Specify a command`);
  process.exit(-1);
}

const commands = {
  build,
  init,
};

const command = commands[commandName];

if (!command) {
  console.error(`  Unknown command: ${commandName}`);
  process.exit(-1);
}

const files = {
  'esy.json':
`{
  "name": "addon",
  "version": "0.1.0",
  "description": "",
  "author": "",
  "license": "MIT",
  "esy": {
    "build": [
      "jbuilder build addon.node"
    ],
    "install": [
      "esy-installer"
    ],
    "buildsInSource": "_build"
  },
  "dependencies": {
    "@esy-ocaml/esy-installer": "^0.0.0",
    "@opam/jbuilder": "^1.0.0-beta16",
    "ocaml-node": "vkurchatkin/ocaml-node"
  },
  "peerDependencies": {
    "ocaml": "~4.6.000"
  },
  "devDependencies": {
    "@opam/merlin": "^3.0.5",
    "ocaml": "~4.6.000"
  },
  "private": true
}
`,

'jbuild-ignore':
`node_modules
`,

'jbuild':
`(jbuild_version 1)

(executable
 ((name addon)
  (public_name addon)
  (libraries (ocaml-node))
  (modes (native shared_object))))

(rule (copy addon.exe addon.node))
`,

'addon.ml':
`open Node;;

let add args =
   let (a,b) = match args with
    | a::b::_ -> ((Value.to_float_exn a), (Value.to_float_exn b))
    | _ -> raise (TypeError ("Wrong number of arguments"))
    in
    Value.of_float (a +. b)
;;

Node.register @@
  fun v  ->
    Object.(
      of_value_exn v
      |> set_f "add" (Function.make add)
      |> to_value
    )
`,

'addon.opam': '',

};

function init() {
  const wd = process.cwd();
  const src = path.relative(wd, 'src');

  if (fs.existsSync(src)) {
    console.log('./src already exists, exiting\n');
    process.exit(-1);
  }

  fs.mkdirSync(src);

  for (const [file, content] of Object.entries(files)) {
    const absPath = path.resolve(wd, 'src', file);

    fs.writeFileSync(absPath, content);
  }

  cp.execSync('esy install', { cwd: src, stdio: 'inherit' });
  cp.execSync('esy build', { cwd: src, stdio: 'inherit' });
}

function build() {
  const wd = process.cwd();
  const src = path.relative(wd, 'src');
  cp.execSync('esy build', { cwd: src, stdio: 'inherit' });
}




command();
