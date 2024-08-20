import inspect
from functools import partial

from beets.dbcore.query import Query
import os

import pytest


def pytest_runtest_setup(item: pytest.Item):
    """Skip integration tests if INTEGRATION_TEST environment variable is not set."""
    if os.environ.get("INTEGRATION_TEST"):
        return

    if next(item.iter_markers(name="integration_test"), None):
        pytest.skip("Skipping integration tests")


def pytest_make_parametrize_id(config, val, argname):
    if argname in {"expected_titles", "expected_albums"}:
        return ""

    if isinstance(val, partial):
        val = val.func

    if inspect.isclass(val) and issubclass(val, Query):
        return val.__name__
    elif isinstance(val, Query):
        return val.__class__.__name__
    elif inspect.isfunction(val) and val.__name__ == "<lambda>":
        return inspect.getsource(val).split("lambda")[-1][:30]

    return repr(val)
