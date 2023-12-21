# Some information to get the LSP stuff running

Tested only with a small fork of the minimal vscode lsp client `vscode-glspc` (git@github.com:threonorm/vscode-glspc.git)

Need to define some settings (from vscode settings pannel):
```
    "glspc.languageId": "bluespec",
    "glspc.serverCommand": "PATHTO/bsc-lsp/inst/bin/bsc_lsp", // Points to the bsc_lsp binary
    "glspc.initializationOptions": {
        "projectFile": "bsclsp.yaml", // This is relative to the workspace, should probably be left alone
    },

```

A `bsclsp.yaml` file should be placed at the root of the workspace.. It specifies the path to the different Bluespec source files and `bo` libraries, and the extra arguments (to define macros for example) to send to the compiler.
As an example, you can refer to `except_bsclsp.yaml`

# Notes

`bsc_lsp` always passes `--aggressive-conditions` to the compiler.

# Dependencies:

Several hackage dependencies:
```
lsp lsp-test text-format yaml concurrent-extra
``` 

For example: 
```sh
cabal v1-install lsp lsp-test text-format yaml concurrent-extra
```

