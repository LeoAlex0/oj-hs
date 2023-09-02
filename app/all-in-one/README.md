# All in one

All in one is a tool to pack all sources in one project to a single source file.

In this repo, it's used to pack answers to submit.

| in `module SubModule` |  in `Combined.hs`   | Comments |
| -------------------------- | ----------------- | -------- |
| `xxxFunc` (not prelude & instance) | `SubModule_xxxFunc` |
| `xxxFunc` (instance typeclass in `SubModule1`) | `SubModule1_xxxFunc` | implement function name in typeclass should match defines in typeclasses |
| `putStrLn` (prelude) | `putStrLn` | prelude module |
| `data U`                   | `data SubModule_U`  |
| `class X`                  | `class SubModule_X` |
| `import SubMoudle as S ... S.a` | `Submodule_a` | import qualifed

## Example

in this example, we haave 2 source file.

Main.hs

```haskell
module Main where

import SelfMod1 (xxx)

xxx :: IO () -- prelude (IO)
main = xxx
```

SelfMod1.hs

```haskell
{-# LANGUAGE GADTs #-} -- extention annots

data WithExt t where
    {-# INLINE Ext1 #-} -- annots with ident name
    Ext1 :: t -> WithExt

xxx :: IO ()
xxx = do
    pure Ext1 1 -- prelude & constatns
```

Combined.hs

```haskell
{-# LANGUAGE GADTs #-} -- from SelfMod1.hs
module Main where

data SelfMod1_WithExt t where -- ident name is prefixed
    {-# INLINE SelfMod1_Ext1 #-} -- annot's ident also prefixed 
    SelfMod1_Ext1 :: t -> SelfMod1_WithExt

SelfMod1_xxx :: IO () -- do nothing with prelude library
SelfMod1_xxx = do
    pure SelfMod1_Ext1 1

main :: IO ()
main = xxx
```
