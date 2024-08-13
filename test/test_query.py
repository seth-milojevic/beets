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
import unittest
from functools import partial

import pytest

import beets.library
from beets import dbcore, util
from beets.dbcore import types
from beets.dbcore.query import (
    InvalidQueryArgumentValueError,
    NoneQuery,
    ParsingError,
)
from beets.library import Item
from beets.test import _common
from beets.test.helper import BeetsTestCase, ItemInDBTestCase, LibMixin
from beets.util import syspath

# Because the absolute path begins with something like C:, we
# can't disambiguate it from an ordinary query.
WIN32_NO_IMPLICIT_PATHS = "Implicit paths are not supported on Windows"


class AnyFieldQueryTest(ItemInDBTestCase):
    def test_no_restriction(self):
        q = dbcore.query.AnyFieldQuery(
            "title",
            beets.library.Item._fields.keys(),
            dbcore.query.SubstringQuery,
        )
        assert self.lib.items(q).get().title == "the title"

    def test_restriction_completeness(self):
        q = dbcore.query.AnyFieldQuery(
            "title", ["title"], dbcore.query.SubstringQuery
        )
        assert self.lib.items(q).get().title == "the title"

    def test_restriction_soundness(self):
        q = dbcore.query.AnyFieldQuery(
            "title", ["artist"], dbcore.query.SubstringQuery
        )
        assert self.lib.items(q).get() is None

    def test_eq(self):
        q1 = dbcore.query.AnyFieldQuery(
            "foo", ["bar"], dbcore.query.SubstringQuery
        )
        q2 = dbcore.query.AnyFieldQuery(
            "foo", ["bar"], dbcore.query.SubstringQuery
        )
        assert q1 == q2

        q2.query_class = None
        assert q1 != q2


# A test case class providing a library with some dummy data and some
# assertions involving that data.
class DummyDataTestCase(BeetsTestCase):
    def setUp(self):
        super().setUp()
        items = [_common.item() for _ in range(3)]
        items[0].title = "foo bar"
        items[0].artist = "one"
        items[0].artists = ["one", "eleven"]
        items[0].album = "baz"
        items[0].year = 2001
        items[0].comp = True
        items[0].genre = "rock"
        items[1].title = "baz qux"
        items[1].artist = "two"
        items[1].artists = ["two", "twelve"]
        items[1].album = "baz"
        items[1].year = 2002
        items[1].comp = True
        items[1].genre = "Rock"
        items[2].title = "beets 4 eva"
        items[2].artist = "three"
        items[2].artists = ["three", "one"]
        items[2].album = "foo"
        items[2].year = 2003
        items[2].comp = False
        items[2].genre = "Hard Rock"
        items[2].comments = "caf\xe9"
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
            ("xyzzy:nonsense", []),
            ("albumflex:foo", ["foo bar", "baz qux"]),
        ],
    )
    def test_get_query(self, lib, q, expected_titles):
        assert {i.title for i in lib.items(q)} == set(expected_titles)


class GetTest(DummyDataTestCase):
    def test_unknown_field_name_no_results_in_album_query(self):
        q = "xyzzy:nonsense"
        results = self.lib.albums(q)
        names = [a.album for a in results]
        assert names == []

    def test_item_field_name_matches_nothing_in_album_query(self):
        q = "format:nonsense"
        results = self.lib.albums(q)
        names = [a.album for a in results]
        assert names == []

    def test_numeric_search_positive(self):
        q = dbcore.query.NumericQuery("year", "2001")
        results = self.lib.items(q)
        assert results

    def test_numeric_search_negative(self):
        q = dbcore.query.NumericQuery("year", "1999")
        results = self.lib.items(q)
        assert not results

    def test_invalid_query(self):
        with pytest.raises(InvalidQueryArgumentValueError, match="not an int"):
            dbcore.query.NumericQuery("year", "199a")

        msg_match = r"not a regular expression.*unterminated subpattern"
        with pytest.raises(ParsingError, match=msg_match):
            dbcore.query.RegexpQuery("year", "199(")


class MatchTest(BeetsTestCase):
    def setUp(self):
        super().setUp()
        self.item = _common.item()

    def test_regex_match_positive(self):
        q = dbcore.query.RegexpQuery("album", "^the album$")
        assert q.match(self.item)

    def test_regex_match_negative(self):
        q = dbcore.query.RegexpQuery("album", "^album$")
        assert not q.match(self.item)

    def test_regex_match_non_string_value(self):
        q = dbcore.query.RegexpQuery("disc", "^6$")
        assert q.match(self.item)

    def test_substring_match_positive(self):
        q = dbcore.query.SubstringQuery("album", "album")
        assert q.match(self.item)

    def test_substring_match_negative(self):
        q = dbcore.query.SubstringQuery("album", "ablum")
        assert not q.match(self.item)

    def test_substring_match_non_string_value(self):
        q = dbcore.query.SubstringQuery("disc", "6")
        assert q.match(self.item)

    def test_exact_match_nocase_positive(self):
        q = dbcore.query.StringQuery("genre", "the genre")
        assert q.match(self.item)
        q = dbcore.query.StringQuery("genre", "THE GENRE")
        assert q.match(self.item)

    def test_exact_match_nocase_negative(self):
        q = dbcore.query.StringQuery("genre", "genre")
        assert not q.match(self.item)

    def test_year_match_positive(self):
        q = dbcore.query.NumericQuery("year", "1")
        assert q.match(self.item)

    def test_year_match_negative(self):
        q = dbcore.query.NumericQuery("year", "10")
        assert not q.match(self.item)

    def test_bitrate_range_positive(self):
        q = dbcore.query.NumericQuery("bitrate", "100000..200000")
        assert q.match(self.item)

    def test_bitrate_range_negative(self):
        q = dbcore.query.NumericQuery("bitrate", "200000..300000")
        assert not q.match(self.item)

    def test_open_range(self):
        dbcore.query.NumericQuery("bitrate", "100000..")

    def test_eq(self):
        q1 = dbcore.query.MatchQuery("foo", "bar")
        q2 = dbcore.query.MatchQuery("foo", "bar")
        q3 = dbcore.query.MatchQuery("foo", "baz")
        q4 = dbcore.query.StringFieldQuery("foo", "bar")
        assert q1 == q2
        assert q1 != q3
        assert q1 != q4
        assert q3 != q4


class TestPathQuery(LibMixin):
    @pytest.fixture(autouse=True, scope="class", name="lib")
    def fixture_lib(self, lib):
        # This is the item we'll try to match.
        self.lib = lib
        i = _common.item(lib)
        i.path = util.normpath("/a/b/c.mp3")
        i.title = "path item"
        i.album = "path album"
        i.store()
        lib.add_album([i])

        # A second item for testing exclusion.
        i2 = _common.item()
        i2.path = util.normpath("/x/y/z.mp3")
        i2.title = "another item"
        i2.album = "another album"
        lib.add(i2)
        lib.add_album([i2])

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
        self.add_album(path=b"/A/B/C2.mp3", title="caps path")

        return lib

    @pytest.fixture
    def _force_implicit_query_detection(self):
        # Unadorned path queries with path separators in them are considered
        # path queries only when the path in question actually exists. So we
        # mock the existence check to return true.
        beets.library.PathQuery.force_implicit_query_detection = True
        yield
        beets.library.PathQuery.force_implicit_query_detection = False

    # @pytest.mark.usefixtures("_force_implicit_query_detection")
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
        assert beets.library.PathQuery.is_path_query(q) == is_path_query


class PathQueryTest(ItemInDBTestCase):
    def setUp(self):
        super().setUp()

        # This is the item we'll try to match.
        self.i.path = util.normpath("/a/b/c.mp3")
        self.i.title = "path item"
        self.i.album = "path album"
        self.i.store()
        self.lib.add_album([self.i])

        # A second item for testing exclusion.
        i2 = _common.item()
        i2.path = util.normpath("/x/y/z.mp3")
        i2.title = "another item"
        i2.album = "another album"
        self.lib.add(i2)
        self.lib.add_album([i2])

    def test_case_sensitivity(self):
        self.add_album(path=b"/A/B/C2.mp3", title="caps path")

        makeq = partial(beets.library.PathQuery, "path", "/A/B")

        results = self.lib.items(makeq(case_sensitive=True))
        assert {i.title for i in results} == {"caps path"}

        results = self.lib.items(makeq(case_sensitive=False))
        assert {i.title for i in results} == {"path item", "caps path"}

    # FIXME: shouldn't this also work on windows?
    @unittest.skipIf(sys.platform == "win32", WIN32_NO_IMPLICIT_PATHS)
    def test_detect_absolute_path(self):
        """Test detection of implicit path queries based on whether or
        not the path actually exists, when using an absolute path query.

        Thus, don't use the `force_implicit_query_detection()`
        contextmanager which would disable the existence check.
        """
        is_path_query = beets.library.PathQuery.is_path_query

        path = self.touch(os.path.join(b"foo", b"bar"))
        assert os.path.isabs(util.syspath(path))
        path_str = path.decode("utf-8")

        # The file itself.
        assert is_path_query(path_str)

        # The parent directory.
        parent = os.path.dirname(path_str)
        assert is_path_query(parent)

        # Some non-existent path.
        assert not is_path_query(f"{path_str}baz")

    def test_detect_relative_path(self):
        """Test detection of implicit path queries based on whether or
        not the path actually exists, when using a relative path query.

        Thus, don't use the `force_implicit_query_detection()`
        contextmanager which would disable the existence check.
        """
        is_path_query = beets.library.PathQuery.is_path_query

        self.touch(os.path.join(b"foo", b"bar"))

        # Temporarily change directory so relative paths work.
        cur_dir = os.getcwd()
        try:
            os.chdir(syspath(self.temp_dir))
            assert is_path_query("foo/")
            assert is_path_query("foo/bar")
            assert is_path_query("foo/bar:tagada")
            assert not is_path_query("bar")
        finally:
            os.chdir(cur_dir)


class IntQueryTest(BeetsTestCase):
    def tearDown(self):
        super().tearDown()
        Item._types = {}

    def test_exact_value_match(self):
        item = self.add_item(bpm=120)
        matched = self.lib.items("bpm:120").get()
        assert item.id == matched.id

    def test_range_match(self):
        item = self.add_item(bpm=120)
        self.add_item(bpm=130)

        matched = self.lib.items("bpm:110..125")
        assert 1 == len(matched)
        assert item.id == matched.get().id

    def test_flex_range_match(self):
        Item._types = {"myint": types.Integer()}
        item = self.add_item(myint=2)
        matched = self.lib.items("myint:2").get()
        assert item.id == matched.id

    def test_flex_dont_match_missing(self):
        Item._types = {"myint": types.Integer()}
        self.add_item()
        matched = self.lib.items("myint:2").get()
        assert matched is None

    def test_no_substring_match(self):
        self.add_item(bpm=120)
        matched = self.lib.items("bpm:12").get()
        assert matched is None


class BoolQueryTest(BeetsTestCase):
    def setUp(self):
        super().setUp()
        Item._types = {"flexbool": types.Boolean()}

    def tearDown(self):
        super().tearDown()
        Item._types = {}

    def test_parse_true(self):
        item_true = self.add_item(comp=True)
        item_false = self.add_item(comp=False)
        matched = self.lib.items("comp:true")
        assert item_true.id in {i.id for i in matched}
        assert item_false.id not in {i.id for i in matched}

    def test_flex_parse_true(self):
        item_true = self.add_item(flexbool=True)
        item_false = self.add_item(flexbool=False)
        matched = self.lib.items("flexbool:true")
        assert item_true.id in {i.id for i in matched}
        assert item_false.id not in {i.id for i in matched}

    def test_flex_parse_false(self):
        item_true = self.add_item(flexbool=True)
        item_false = self.add_item(flexbool=False)
        matched = self.lib.items("flexbool:false")
        assert item_false.id in {i.id for i in matched}
        assert item_true.id not in {i.id for i in matched}

    def test_flex_parse_1(self):
        item_true = self.add_item(flexbool=True)
        item_false = self.add_item(flexbool=False)
        matched = self.lib.items("flexbool:1")
        assert item_true.id in {i.id for i in matched}
        assert item_false.id not in {i.id for i in matched}

    def test_flex_parse_0(self):
        item_true = self.add_item(flexbool=True)
        item_false = self.add_item(flexbool=False)
        matched = self.lib.items("flexbool:0")
        assert item_false.id in {i.id for i in matched}
        assert item_true.id not in {i.id for i in matched}

    def test_flex_parse_any_string(self):
        # TODO this should be the other way around
        item_true = self.add_item(flexbool=True)
        item_false = self.add_item(flexbool=False)
        matched = self.lib.items("flexbool:something")
        assert item_false.id in {i.id for i in matched}
        assert item_true.id not in {i.id for i in matched}


class DefaultSearchFieldsTest(DummyDataTestCase):
    def test_albums_matches_album(self):
        albums = list(self.lib.albums("baz"))
        assert len(albums) == 1

    def test_albums_matches_albumartist(self):
        albums = list(self.lib.albums(["album artist"]))
        assert len(albums) == 1

    def test_items_matches_title(self):
        assert self.lib.items("beets")

    def test_items_does_not_match_year(self):
        assert not self.lib.items("2001")


class NoneQueryTest(BeetsTestCase):
    def test_match_singletons(self):
        singleton = self.add_item()
        album_item = self.add_album().items().get()

        matched = self.lib.items(NoneQuery("album_id"))
        assert singleton.id in {i.id for i in matched}
        assert album_item.id not in {i.id for i in matched}

    def test_match_after_set_none(self):
        item = self.add_item(rg_track_gain=0)
        matched = self.lib.items(NoneQuery("rg_track_gain"))
        assert item.id not in {i.id for i in matched}

        item["rg_track_gain"] = None
        item.store()
        matched = self.lib.items(NoneQuery("rg_track_gain"))
        assert item.id in {i.id for i in matched}

    def test_match_slow(self):
        item = self.add_item()
        matched = self.lib.items(NoneQuery("rg_track_peak", fast=False))
        assert item.id in {i.id for i in matched}

    def test_match_slow_after_set_none(self):
        item = self.add_item(rg_track_gain=0)
        matched = self.lib.items(NoneQuery("rg_track_gain", fast=False))
        assert item.id not in {i.id for i in matched}

        item["rg_track_gain"] = None
        item.store()
        matched = self.lib.items(NoneQuery("rg_track_gain", fast=False))
        assert item.id in {i.id for i in matched}


class NotQueryMatchTest(BeetsTestCase):
    """Test `query.NotQuery` matching against a single item, using the same
    cases and assertions as on `MatchTest`, plus assertion on the negated
    queries (ie. assert q -> assert not NotQuery(q)).
    """

    def setUp(self):
        super().setUp()
        self.item = _common.item()

    def test_regex_match_positive(self):
        q = dbcore.query.RegexpQuery("album", "^the album$")
        assert q.match(self.item)
        assert not dbcore.query.NotQuery(q).match(self.item)

    def test_regex_match_negative(self):
        q = dbcore.query.RegexpQuery("album", "^album$")
        assert not q.match(self.item)
        assert dbcore.query.NotQuery(q).match(self.item)

    def test_regex_match_non_string_value(self):
        q = dbcore.query.RegexpQuery("disc", "^6$")
        assert q.match(self.item)
        assert not dbcore.query.NotQuery(q).match(self.item)

    def test_substring_match_positive(self):
        q = dbcore.query.SubstringQuery("album", "album")
        assert q.match(self.item)
        assert not dbcore.query.NotQuery(q).match(self.item)

    def test_substring_match_negative(self):
        q = dbcore.query.SubstringQuery("album", "ablum")
        assert not q.match(self.item)
        assert dbcore.query.NotQuery(q).match(self.item)

    def test_substring_match_non_string_value(self):
        q = dbcore.query.SubstringQuery("disc", "6")
        assert q.match(self.item)
        assert not dbcore.query.NotQuery(q).match(self.item)

    def test_year_match_positive(self):
        q = dbcore.query.NumericQuery("year", "1")
        assert q.match(self.item)
        assert not dbcore.query.NotQuery(q).match(self.item)

    def test_year_match_negative(self):
        q = dbcore.query.NumericQuery("year", "10")
        assert not q.match(self.item)
        assert dbcore.query.NotQuery(q).match(self.item)

    def test_bitrate_range_positive(self):
        q = dbcore.query.NumericQuery("bitrate", "100000..200000")
        assert q.match(self.item)
        assert not dbcore.query.NotQuery(q).match(self.item)

    def test_bitrate_range_negative(self):
        q = dbcore.query.NumericQuery("bitrate", "200000..300000")
        assert not q.match(self.item)
        assert dbcore.query.NotQuery(q).match(self.item)

    def test_open_range(self):
        q = dbcore.query.NumericQuery("bitrate", "100000..")
        dbcore.query.NotQuery(q)


class TestNotQuery:
    """Test `query.NotQuery` against the dummy data."""

    @pytest.fixture(autouse=True, scope="class")
    def lib(self):
        test_case = DummyDataTestCase()
        test_case.setUp()
        return test_case.lib

    @pytest.mark.parametrize(
        "q, expected_results",
        [
            (
                dbcore.query.BooleanQuery("comp", True),
                {"beets 4 eva"},
            ),
            (
                dbcore.query.DateQuery("added", "2000-01-01"),
                {"foo bar", "baz qux", "beets 4 eva"},
            ),
            (
                dbcore.query.FalseQuery(),
                {"foo bar", "baz qux", "beets 4 eva"},
            ),
            (
                dbcore.query.MatchQuery("year", "2003"),
                {"foo bar", "baz qux"},
            ),
            (
                dbcore.query.NoneQuery("rg_track_gain"),
                set(),
            ),
            (
                dbcore.query.NumericQuery("year", "2001..2002"),
                {"beets 4 eva"},
            ),
            (
                dbcore.query.AnyFieldQuery(
                    "baz", ["album"], dbcore.query.MatchQuery
                ),
                {"beets 4 eva"},
            ),
            (
                dbcore.query.AndQuery(
                    [
                        dbcore.query.BooleanQuery("comp", True),
                        dbcore.query.NumericQuery("year", "2002"),
                    ]
                ),
                {"foo bar", "beets 4 eva"},
            ),
            (
                dbcore.query.OrQuery(
                    [
                        dbcore.query.BooleanQuery("comp", True),
                        dbcore.query.NumericQuery("year", "2002"),
                    ]
                ),
                {"beets 4 eva"},
            ),
            (
                dbcore.query.RegexpQuery("artist", "^t"),
                {"foo bar"},
            ),
            (
                dbcore.query.SubstringQuery("album", "ba"),
                {"beets 4 eva"},
            ),
            (
                dbcore.query.TrueQuery(),
                set(),
            ),
        ],
        ids=lambda x: x.__class__ if isinstance(x, dbcore.query.Query) else "",
    )
    def test_query_type(self, lib, q, expected_results):
        print(q)

        def get_results(*args):
            return {i.title for i in lib.items(*args)}

        # not(a and b) <-> not(a) or not(b)
        not_q = dbcore.query.NotQuery(q)
        not_q_results = get_results(not_q)
        assert not_q_results == expected_results

        # assert using OrQuery, AndQuery
        q_or = dbcore.query.OrQuery([q, not_q])

        q_and = dbcore.query.AndQuery([q, not_q])
        assert get_results(q_or) == {"foo bar", "baz qux", "beets 4 eva"}
        assert get_results(q_and) == set()

        # assert manually checking the item titles
        all_titles = get_results()
        q_results = get_results(q)
        assert q_results.union(not_q_results) == all_titles
        assert q_results.intersection(not_q_results) == set()

        # round trip
        not_not_q = dbcore.query.NotQuery(not_q)
        assert get_results(q) == get_results(not_not_q)


class TestNegationPrefix:
    @pytest.fixture(autouse=True, scope="class")
    def lib(self):
        test_case = DummyDataTestCase()
        test_case.setUp()
        return test_case.lib

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


class FastVsSlowTest(DummyDataTestCase):
    """Tests negation prefixes."""

    def test_fast_vs_slow(self):
        """Test that the results are the same regardless of the `fast` flag
        for negated `FieldQuery`s.

        TODO: investigate NoneQuery(fast=False), as it is raising
        AttributeError: type object 'NoneQuery' has no attribute 'field'
        at NoneQuery.match() (due to being @classmethod, and no self?)
        """
        classes = [
            (dbcore.query.DateQuery, ["added", "2001-01-01"]),
            (dbcore.query.MatchQuery, ["artist", "one"]),
            # (dbcore.query.NoneQuery, ['rg_track_gain']),
            (dbcore.query.NumericQuery, ["year", "2002"]),
            (dbcore.query.StringFieldQuery, ["year", "2001"]),
            (dbcore.query.RegexpQuery, ["album", "^.a"]),
            (dbcore.query.SubstringQuery, ["title", "x"]),
        ]

        for klass, args in classes:
            q_fast = dbcore.query.NotQuery(klass(*(args + [True])))
            q_slow = dbcore.query.NotQuery(klass(*(args + [False])))

            try:
                assert [i.title for i in self.lib.items(q_fast)] == [
                    i.title for i in self.lib.items(q_slow)
                ]
            except NotImplementedError:
                # ignore classes that do not provide `fast` implementation
                pass


class TestRelatedQueries:
    """Test album-level queries with track-level filters and vice-versa."""

    @pytest.fixture(scope="class")
    def lib(self, lib):
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
