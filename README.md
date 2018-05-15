# Temporary Merge Tool (tmt)

This is intended to be a tool that, in effect, allows several
git branches to be checked out at once into the same repo, with
changes committed onto a specific branch.

A use case is to try out someone else's feature branches alongside
your feature branches, without permanently tangling them together.

# Comparison to stgit

I like using stgit (https://github.com/ctmarinas/stgit) to keep
different pieces of work separate (as patches) while also being able
to use my local copy of a repo with several (or all) of those patches
applied.

That doesn't work so well for collaborative development - stgit patches
aren't themselves versioned, so are hard to share.

tmt is an attempt to get something like that workflow, but
collaboratively. The equivalent to an stgit patch is a regular git
branch. 'tmt' lets you combine a bunch of those regular branches
into your work directory, and commit a change onto any of those
branches. Regular git commands let you share those branches with
other developers.

# License

    tmt is Copyright (C) 2018 Ben Clifford

    This program is free software; you can redistribute it and/or modify
    it under the terms of version 2 of the GNU General Public License
    as published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335 USA

