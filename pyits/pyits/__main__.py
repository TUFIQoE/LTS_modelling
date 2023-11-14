# SPDX-FileCopyrightText: 2023-present Krzysztof Rusek <krusek@agh.edu.pl>
#
# SPDX-License-Identifier: MIT
import argparse
import dataclasses
from dataclasses import dataclass, fields, MISSING, field
import logging

import gsd
import pandas as pd
import numpy as np
import jax
import jax.numpy as jnp
from jax import Array
from jax.typing import ArrayLike


@dataclass
class ExperimentMatrix:
    """
    Sparse representation of the test results
    """
    score: Array
    pvs: Array
    tester: Array
    # shape
    n_pvs: int
    n_tester: int

    def to_dense(self):
        o = self
        z = jnp.full((o.n_pvs, o.n_tester))
        z = z.at[o.pvs, o.tester].set(o.score)
        return z


@dataclass
class Arguments:
    """GSD Its

    Attributes:
        inp CSV
        out  outputfile
    """
    inp: str
    out: str = 'log/out.csv'

    @classmethod
    def parse(cls):
        parser = argparse.ArgumentParser(description=cls.__doc__)
        fields_info = fields(cls)
        for field in fields_info:
            default = None if field.default == MISSING else field.default
            parser.add_argument('--' + field.name, type=field.type, default=default)

        args = parser.parse_args()
        return cls(**args.__dict__)


if __name__ == '__main__':
    args = Arguments.parse()

    logging.basicConfig(format='%(asctime)s %(levelname)s:%(message)s', level=logging.INFO)

    logging.info(args)
    logging.info(jax.devices())

    df = pd.read_csv(args.inp)

    infos = []

    #hdtv
    for eid in range(1,7):
        dfe = df.loc[df.Experiment == eid, :]

        pvss = dfe['PVS_id'].unique()

        for pvs in pvss:
            arr = dfe.loc[dfe.PVS_id == pvs, 'Score'].to_numpy()
            counts_arr = jnp.sum(arr == np.expand_dims(np.arange(1, 6), 1), axis=1)

            hat, opt_state = gsd.fit_mle(counts_arr)

            fhat = jax.tree_util.tree_map(float, hat)
            info = dict(eid=eid, pvs=pvs, **fhat._asdict())
            print(info)
            infos.append(info)

    pd.DataFrame(infos).to_csv(args.out)

