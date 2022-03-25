# treeish

`treeish` is a package for comparing two treelike structures and getting a normalised
comparison between them. The comparison is intended to be somewhat rough, to get a
qualitative judgment: changed spelling or an added sentence in a text field should
not trigger an alarm, but a missing node ought to be flagged.

Also need to be able to take a set of diffs and collapse them into a single diff.
This lets us take a set of rows with a key field and compare two whole datasets.

## questions

- does level in the tree matter when it comes to weighting differences?
  For my purposes, my trees are not deep so it doesn't matter much, but it'd
  be nice to have a principled answer. For the moment, I'll flatten it out.
