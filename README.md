# Haskell-RecordBuilder

Generating getter/setter boilerplates for Haskell data structures.

## Example

See [here](Example/Example1.hs) for an example.

## Usage

Assuming you already have installed `ghc` and `cabal`. At the root folder, run `cabal run record-builder -v0 <file_name.hs>`, where `<file_name.hs>` is the Haskell source code containing data structures that you would like to have some getters and setters for.

The boilerplates contain the following for each data structure defined in the source code. For the purpose of illustration, we assume the data is in the form `data Tree a = Empty | Leaf { value :: a } | Node { left :: Tree a, value :: a, right :: value a }`.

1. Constructor checkers: checks if the argument has the desired constructor. For example, the function `isLeaf :: Tree a -> Bool` tells if the `Tree a` has a `Leaf` constructor.

2. Getters for each record field: get the desired field from the argument, returning `Nothing` if the field does not exist for the constructor. For example, the function `getValue :: Tree a -> Maybe a` returns the `value` field. Note that it works only for fields with explicit records.

3. Setters for each record field: takes an instance and a new value, update the desired field with the value. If the field does not exist for the constructor, it returns the same instance. For example, the function `setValue :: Tree a -> a -> Tree a` updates the `value` field. Note that it works only for fields with explicit records.

4. Modifiers for each record field: takes an instance and a transforming function, apply the function on the value of the desired field and return a new instance. For example, the function `setValue :: Tree a -> (a -> a) -> Tree a` updates the `value` field by the given function. Note that it works only for fields with explicit records.

5. Strict modifiers: similar to the modifiers above, but strictly evaluates the old value to its WHNF before applying the function on it.

The output will be directed to stdout; you can pipe it to any file.
