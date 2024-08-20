# This file is part of beets.
# Copyright 2016, Adrian Sampson.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

"""Various tests for querying the library database."""

import os
import sys
from functools import partial

import pytest

from beets import util
from beets.dbcore import types
from beets.dbcore.query import (
    AndQuery,
    AnyFieldQuery,
    BooleanQuery,
    DateQuery,
    FalseQuery,
    MatchQuery,
    NoneQuery,
    NotQuery,
    NumericQuery,
    OrQuery,
    ParsingError,
    RegexpQuery,
    StringFieldQuery,
    StringQuery,
    SubstringQuery,
    TrueQuery,
)
from beets.library import Item, PathQuery
from beets.test import _common
from beets.test.helper import (
    BeetsTestCase,
    LibMixin,
    TestHelper,
)

# Because the absolute path begins with something like C:, we
# can't disambiguate it from an ordinary query.
WIN32_NO_IMPLICIT_PATHS = "Implicit paths are not supported on Windows"


AnySubstringQuery = partial(AnyFieldQuery, cls=SubstringQuery)


@pytest.mark.parametrize(
    "query_class", [MatchQuery, StringFieldQuery, AnySubstringQuery]
)
def test_equality(query_class):
    assert query_class("foo", "bar") == query_class("foo", "bar")


# A test case class providing a library with some dummy data and some
# assertions involving that data.
class DummyDataTestCase(BeetsTestCase):
    def setUp(self):
        super().setUp()
        items = [
            _common.item(
                title="foo bar",
                artist="one",
                artists=["one", "eleven"],
                album="baz",
                year=2001,
                comp=True,
                genre="rock",
            ),
            _common.item(
                title="baz qux",
                artist="two",
                artists=["two", "twelve"],
                album="baz",
                year=2002,
                comp=True,
                genre="Rock",
            ),
            _common.item(
                title="beets 4 eva",
                artist="three",
                artists=["three", "one"],
                album="foo",
                year=2003,
                comp=False,
                genre="Hard Rock",
                comments="caf\xe9",
            ),
        ]
        for item in items:
            self.lib.add(item)
        self.album = self.lib.add_album(items[:2])
        self.album.albumflex = "foo"
        self.album.store()


@pytest.fixture(scope="class")
def lib():
    test_case = DummyDataTestCase()
    test_case.setUp()
    return test_case.lib


class TestGet:
    @pytest.mark.parametrize(
        "q, expected_titles",
        [
            ("", ["foo bar", "baz qux", "beets 4 eva"]),
            (None, ["foo bar", "baz qux", "beets 4 eva"]),
            ("comments:caf\xe9", ["beets 4 eva"]),
            ("title:qux", ["baz qux"]),
            ("genre:=rock", ["foo bar"]),
            ("genre:=Rock", ["baz qux"]),
            ('genre:="Hard Rock"', ["beets 4 eva"]),
            ('genre:=~"hard rock"', ["beets 4 eva"]),
            ("artist::t.+r", ["beets 4 eva"]),
            ("three", ["beets 4 eva"]),
            ("=rock", ["foo bar"]),
            ('=~"hard rock"', ["beets 4 eva"]),
            (":x$", ["baz qux"]),
            ("popebear", []),
            ("pope:bear", []),
            ('genre:="hard rock"', []),
            ("oNE", ["foo bar"]),
            (":oNE", []),
            (":one", ["foo bar"]),
            ("artist:thrEE", ["beets 4 eva"]),
            ("artists::eleven", ["foo bar"]),
            ("artists::one", ["foo bar", "beets 4 eva"]),
            ("ArTiST:three", ["beets 4 eva"]),
            ("genre:=~rock", ["foo bar", "baz qux"]),
            ("baz", ["foo bar", "baz qux"]),
            (":z$", ["foo bar", "baz qux"]),
            ("title:baz", ["baz qux"]),
            ("title::baz", ["baz qux"]),
            ("qux baz", ["baz qux"]),
            (":baz :qux", ["baz qux"]),
            (":baz qux", ["baz qux"]),
            ("year:2001", ["foo bar"]),
            ("year:2000..2002", ["foo bar", "baz qux"]),
            ("singleton:true", ["beets 4 eva"]),
            ("singleton:1", ["beets 4 eva"]),
            ("singleton:false", ["foo bar", "baz qux"]),
            ("singleton:0", ["foo bar", "baz qux"]),
            ("comp:true", ["foo bar", "baz qux"]),
            ("comp:false", ["beets 4 eva"]),
            pytest.param("xyzzy:nonsense", [], id="unknown-field"),
            ("albumflex:foo", ["foo bar", "baz qux"]),
        ],
    )
    def test_get_query(self, lib, q, expected_titles):
        assert {i.title for i in lib.items(q)} == set(expected_titles)

    @pytest.mark.parametrize(
        "make_q, expected_msg",
        [
            (lambda: NumericQuery("year", "199a"), "not an int"), 
            (lambda: RegexpQuery("year", "199("), r"not a regular expression.*unterminated subpattern"),
        ]
    )  # fmt: skip
    def test_invalid_query(self, make_q, expected_msg):
        with pytest.raises(ParsingError, match=expected_msg):
            make_q()


class TestMatch:
    @pytest.fixture(scope="class")
    def item(self):
        return _common.item(
            album="the album",
            disc=6,
            genre="the genre",
            year=1,
            bitrate=128000,
        )

    @pytest.mark.parametrize(
        "q, should_match",
        [
            (RegexpQuery("album", "^the album$"), True),
            (RegexpQuery("album", "^album$"), False),
            (RegexpQuery("disc", "^6$"), True),
            (SubstringQuery("album", "album"), True),
            (SubstringQuery("album", "ablum"), False),
            (SubstringQuery("disc", "6"), True),
            (StringQuery("genre", "the genre"), True),
            (StringQuery("genre", "THE GENRE"), True),
            (StringQuery("genre", "genre"), False),
            (NumericQuery("year", "1"), True),
            (NumericQuery("year", "10"), False),
            (NumericQuery("bitrate", "100000..200000"), True),
            (NumericQuery("bitrate", "200000..300000"), False),
            (NumericQuery("bitrate", "100000.."), True),
        ],
    )
    def test_match(self, item, q, should_match):
        assert q.match(item) == should_match
        assert not NotQuery(q).match(item) == should_match


class TestPathQuery(LibMixin):
    @pytest.fixture(autouse=True, scope="class", name="lib")
    def fixture_lib(self, lib):
        # This is the item we'll try to match.
        self.lib = lib
        self.add_album(
            path=util.normpath("/a/b/c.mp3"),
            title="path item",
            album="path album",
        )
        # A second item for testing exclusion.
        self.add_album(
            path=util.normpath("/x/y/z.mp3"),
            title="another item",
            album="another album",
        )
        self.add_album(
            path=b"/c/_/title.mp3",
            title="with underscore",
            album="album with underscore",
        )
        self.add_album(
            path=b"/c/%/title.mp3",
            title="with percent",
            album="album with percent",
        )
        self.add_album(
            path=rb"/c/\x/title.mp3",
            title="with backslash",
            album="album with backslash",
        )
        self.add_album(path=b"/A/B/C2.mp3")

        return lib

    @pytest.fixture
    def _force_implicit_query_detection(self):
        # Unadorned path queries with path separators in them are considered
        # path queries only when the path in question actually exists. So we
        # mock the existence check to return true.
        PathQuery.force_implicit_query_detection = True
        yield
        PathQuery.force_implicit_query_detection = False

    @pytest.mark.parametrize(
        "q, expected_titles, expected_albums",
        [
            ("path:/a/b/c.mp3", ["path item"], ["path album"]),
            ("path:/a", ["path item"], ["path album"]),
            ("path:/a/", ["path item"], ["path album"]),
            ("path:/xyzzy/", [], []),
            ("path:/b/", [], []),
            ("path:/x/../a/b", ["path item"], ["path album"]),
            ("/a/b , /a/b", [], []),
            ("path::c\\.mp3$", ["path item"], ["path album"]),
            ("path::b", ["path item"], ["path album"]),
            ("path:/c/_", ["with underscore"], ["album with underscore"]),
            ("path:/c/%", ["with percent"], ["album with percent"]),
            ("path:/c/\\\\x", ["with backslash"], ["album with backslash"]),
        ],
    )
    def test_path_query(self, lib, q, expected_titles, expected_albums):
        assert {i.album for i in lib.albums(q)} == set(expected_albums)
        assert {i.title for i in lib.items(q)} == set(expected_titles)

    @pytest.mark.skipif(sys.platform == "win32", reason=WIN32_NO_IMPLICIT_PATHS)
    @pytest.mark.usefixtures("_force_implicit_query_detection")
    @pytest.mark.parametrize(
        "q, expected_titles, expected_albums",
        [
            ("path:/a/b", ["path item"], ["path album"]),
            ("c.mp3", [], []),
            ("title:/a/b", [], []),
        ],
    )
    def test_implicit_path_query(
        self, lib, q, expected_titles, expected_albums
    ):
        assert {i.title for i in lib.items(q)} == set(expected_titles)
        assert {i.album for i in lib.albums(q)} == set(expected_albums)

    # FIXME: Also create a variant of this test for windows, which tests
    # both os.sep and os.altsep
    @pytest.mark.skipif(sys.platform == "win32", reason=WIN32_NO_IMPLICIT_PATHS)
    @pytest.mark.usefixtures("_force_implicit_query_detection")
    @pytest.mark.parametrize(
        "q, is_path_query",
        [
            ("/foo/bar", True),
            ("foo/bar", True),
            ("foo/", True),
            ("foo", False),
            ("foo/:bar", True),
            ("foo:bar/", False),
            ("foo:/bar", False),
        ],
    )
    def test_path_sep_detection(self, q, is_path_query):
        assert PathQuery.is_path_query(q) == is_path_query

    # FIXME: shouldn't this also work on windows?
    @pytest.mark.skipif(sys.platform == "win32", reason=WIN32_NO_IMPLICIT_PATHS)
    def test_detect_absolute_path(self, tmp_path):
        """Test detection of implicit path queries based on whether or
        not the path actually exists, when using an absolute path query.

        Thus, don't use the `force_implicit_query_detection()`
        contextmanager which would disable the existence check.
        """
        is_path_query = PathQuery.is_path_query

        test_dir = tmp_path
        path = test_dir / "foo" / "bar"
        path.parent.mkdir()
        path.touch()

        assert os.path.isabs(util.syspath(path))
        path_str = str(path)

        # The file itself.
        assert is_path_query(path_str)

        # The parent directory.
        assert is_path_query(os.path.dirname(path_str))

        # Some non-existent path.
        assert not is_path_query(f"{path_str}baz")

    def test_detect_relative_path(self, tmp_path, monkeypatch):
        """Test detection of implicit path queries based on whether or
        not the path actually exists, when using a relative path query.

        Thus, don't use the `force_implicit_query_detection()`
        contextmanager which would disable the existence check.
        """
        is_path_query = PathQuery.is_path_query

        test_dir = tmp_path
        (test_dir / "foo").mkdir()
        (test_dir / "foo" / "bar").touch()
        monkeypatch.chdir(test_dir)
        test_dir.write_text

        assert is_path_query("foo/")
        assert is_path_query("foo/bar")
        assert is_path_query("foo/bar:tagada")
        assert not is_path_query("bar")


class TestQuery(TestHelper):
    YES = "yes"
    NO = "no"

    @pytest.fixture(autouse=True, scope="class", name="lib")
    def fixture_lib(self):
        Item._types = {
            "flexbool": types.Boolean(),
            "flexint": types.Integer(),
        }
        self.setup_beets()
        first = self.add_item(
            title=self.YES,
            comp=True,
            flexbool=True,
            bpm=120,
            flexint=2,
            path=b"A.MP3",
            album="album",
            albumartist="albumartist",
            rg_track_gain=0,
        )
        self.lib.add_album([first])
        self.add_item(
            title=self.NO,
            comp=False,
            flexbool=False,
            path=b"a.mp3",
            rg_track_gain=None,
        )

        yield self.lib

        Item._types = {}

    @pytest.mark.parametrize(
        "q, expected_titles",
        [  # fmt: skip
            pytest.param("comp:true", {YES}, id="parse-true"),
            pytest.param("flexbool:true", {YES}, id="flex-parse-true"),
            pytest.param("flexbool:false", {NO}, id="flex-parse-false"),
            pytest.param("flexbool:1", {YES}, id="flex-parse-1"),
            pytest.param("flexbool:0", {NO}, id="flex-parse-0"),
            # TODO this should be the other way around
            pytest.param("flexbool:something", {NO}, id="flex-parse-true"),
            pytest.param("bpm:120", {YES}, id="int-exact-value"),
            pytest.param("bpm:110..125", {YES}, id="int-range"),
            pytest.param("flexint:2", {YES}, id="int-flex"),
            pytest.param("flexint:3", set(), id="int-no-match"),
            pytest.param("bpm:12", set(), id="int-dont-match-substring"),
            pytest.param(AnySubstringQuery(YES, ["title"]), {YES}, id="any-field-match"),
            pytest.param(AnySubstringQuery(YES, ["artist"]), set(), id="any-field-no-match"),
            pytest.param(PathQuery("path", "A.MP3", case_sensitive=True), {YES}, id="path-match-upper-only"),
            pytest.param(PathQuery("path", "A.MP3", case_sensitive=False), {YES, NO}, id="path-match-any-case"),
            pytest.param(NoneQuery("album_id"), {NO}, id="none-match-singleton"),
            pytest.param(NoneQuery("rg_track_gain"), {NO}, id="none-match-none-value"),
         ]
    )  # fmt: skip
    def test_path_query(self, lib, q, expected_titles):
        assert {i.title for i in lib.items(q)} == expected_titles


class TestDefaultSearchFields(TestHelper):
    @pytest.fixture(autouse=True, scope="class", name="lib")
    def fixture_lib(self):
        self.setup_beets()

        item = self.add_item(
            title="title",
            album="album",
            albumartist="albumartist",
        )
        self.lib.add_album([item])

        yield self.lib

    @pytest.mark.parametrize(
        "entity, q, should_match",
        [
            pytest.param("albums", "album", True, id="album-match-album"),
            pytest.param("albums", "albumartist", True, id="album-match-albumartist"),
            pytest.param("items", "title", True, id="item-match-title"),
            pytest.param("items", "2001", False, id="item-dont-match-year"),
        ],
    )  # fmt: skip
    def test_albums(self, lib, entity, q, should_match):
        assert bool(getattr(lib, entity)(q)) == should_match


class TestNotQuery:
    """Test `query.NotQuery` against the dummy data."""

    @pytest.mark.parametrize(
        "q, expected_titles",
        [
            (BooleanQuery("comp", True), {"beets 4 eva"}),
            (
                DateQuery("added", "2000-01-01"),
                {"foo bar", "baz qux", "beets 4 eva"},
            ),
            (FalseQuery(), {"foo bar", "baz qux", "beets 4 eva"}),
            (MatchQuery("year", "2003"), {"foo bar", "baz qux"}),
            (NoneQuery("rg_track_gain"), set()),
            (NumericQuery("year", "2001..2002"), {"beets 4 eva"}),
            (AnyFieldQuery("baz", ["album"], MatchQuery), {"beets 4 eva"}),
            (
                AndQuery(
                    [BooleanQuery("comp", True), NumericQuery("year", "2002")]
                ),
                {"foo bar", "beets 4 eva"},
            ),
            (
                OrQuery(
                    [BooleanQuery("comp", True), NumericQuery("year", "2002")]
                ),
                {"beets 4 eva"},
            ),
            (RegexpQuery("artist", "^t"), {"foo bar"}),
            (SubstringQuery("album", "ba"), {"beets 4 eva"}),
            (TrueQuery(), set()),
        ],
    )
    def test_query_type(self, lib, q, expected_titles):
        def get_results(*args):
            return {i.title for i in lib.items(*args)}

        # not(a and b) <-> not(a) or not(b)
        not_q = NotQuery(q)
        not_q_results = get_results(not_q)
        assert not_q_results == expected_titles

        # assert using OrQuery, AndQuery
        q_or = OrQuery([q, not_q])

        q_and = AndQuery([q, not_q])
        assert get_results(q_or) == {"foo bar", "baz qux", "beets 4 eva"}
        assert get_results(q_and) == set()

        # assert manually checking the item titles
        all_titles = get_results()
        q_results = get_results(q)
        assert q_results.union(not_q_results) == all_titles
        assert q_results.intersection(not_q_results) == set()

        # round trip
        not_not_q = NotQuery(not_q)
        assert get_results(q) == get_results(not_not_q)


class TestNegationPrefix:
    @pytest.mark.parametrize(
        "q, expected_titles",
        [
            ("-artist::t.+r", ["foo bar", "baz qux"]),
            ("-:x$", ["foo bar", "beets 4 eva"]),
            ("baz -bar", ["baz qux"]),
            ("baz -title:bar", ["baz qux"]),
            ("-qux", ["foo bar", "beets 4 eva"]),
            ("^qux", ["foo bar", "beets 4 eva"]),
            ("^title:qux", ["foo bar", "beets 4 eva"]),
            ("-title:qux", ["foo bar", "beets 4 eva"]),
        ],
    )
    def test_get_query(self, lib, q, expected_titles):
        actual_titles = {i.title for i in lib.items(q)}
        assert actual_titles == set(expected_titles)


class TestFastVsSlow:
    """Tests negation prefixes."""

    @pytest.mark.parametrize(
        "make_q",
        [
            partial(DateQuery, "added", "2001-01-01"),
            partial(MatchQuery, "artist", "one"),
            partial(NoneQuery, "rg_track_gain"),
            partial(NumericQuery, "year", "2002"),
            partial(RegexpQuery, "album", "^.a"),
            partial(SubstringQuery, "title", "x"),
        ],
    )
    def test_fast_vs_slow(self, lib, make_q):
        """Test that the results are the same regardless of the `fast` flag
        for negated `FieldQuery`s.

        TODO: investigate NoneQuery(fast=False), as it is raising
        AttributeError: type object 'NoneQuery' has no attribute 'field'
        at NoneQuery.match() (due to being @classmethod, and no self?)
        """
        q_fast = make_q(True)
        q_slow = make_q(False)

        assert list(map(dict, lib.items(q_fast))) == list(
            map(dict, lib.items(q_slow))
        )


class TestRelatedQueries:
    """Test album-level queries with track-level filters and vice-versa."""

    @pytest.fixture(scope="class", name="lib")
    def fixture_lib(self, lib):
        albums = []
        for album_idx in range(1, 3):
            album_name = f"Album{album_idx}"
            album_items = []
            for item_idx in range(1, 3):
                item = _common.item()
                item.album = album_name
                item.title = f"{album_name} Item{item_idx}"
                lib.add(item)
                album_items.append(item)
            album = lib.add_album(album_items)
            album.artpath = f"{album_name} Artpath"
            album.catalognum = "ABC"
            album.store()
            albums.append(album)

        return lib

    @pytest.mark.parametrize(
        "q, expected_titles, expected_albums",
        [
            ("title:Album1", ["Album1 Item1", "Album1 Item2"], ["Album1"]),
            ("artpath::Album1", ["Album1 Item1", "Album1 Item2"], ["Album1"]),
            (
                "catalognum:ABC Album1",
                ["Album1 Item1", "Album1 Item2"],
                ["Album1"],
            ),
        ],
    )
    def test_path_query(self, lib, q, expected_titles, expected_albums):
        assert {i.album for i in lib.albums(q)} == set(expected_albums)
        assert {i.title for i in lib.items(q)} == set(expected_titles)
