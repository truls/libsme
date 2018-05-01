#!/usr/bin/env python3
from sme import Network, SimulationProcess, Bus, SME, ExternalBus, extends
#from sme import ConstrainedTypes as t
#from sme import Types as t
#from baseavg import *
import numpy as np
from matplotlib import pyplot as plt
import pylab
import sys

class Source(SimulationProcess):
    def setup(self, ins, outs, data, count):
        self.map_outs(outs, "out")
        self.data = data
        self.count = count
        self.gen = self._datagen()

    def _datagen(self):
        for e in self.data:
            yield e

    def run(self):
        self.out["val"] = next(self.gen)
        self.out["valid"] = True


class Logger(SimulationProcess):
    def setup(self, ins, outs, count, results):
        self.map_ins(ins, "data")
        self.results = results
        self.curpos = 0

    def run(self):
        if self.data["valid"]:
            print(self.results.shape)
            self.results[0][self.curpos] = self.data["long"]
            self.results[1][self.curpos] = self.data["short"]
        self.curpos += 1


@extends("ewma.sme", ["-t", "trace.csv", "--force"])
class EWMA(Network):
    def wire(self, indata, outdata):
        count = len(indata)

        data_in = ExternalBus("stream")
        data_out = ExternalBus("output")
        self.add(data_in)
        self.add(data_out)
        logger = Logger("Logger", [data_out], [], count, outdata)
        self.add(logger)
        source = Source("Source", [], [data_in], indata, count)
        self.add(source)


def main():
    sme = SME()
    if len(sme.remaining_options) != 1:
        print("I need a single inputfile argument")
        return 1

    with open(sme.remaining_options[0], 'rb') as f:
        indata = np.load(f)
        indata = np.rint(indata*1000).astype(np.int64)
    #print(indata.shape)
    print(indata.shape[0])
    outdata = np.zeros((2,indata.shape[0]))
    print(outdata.shape)
    #print(outdata.shape)
    sme.network = EWMA("EWMA", "foo", indata, outdata)
    sme.network.clock(255)
    #print(outdata)
    x = np.linspace(0, 0.5, len(indata))
    plt.plot(x, indata, '0.7', x, outdata[0], '0.5', x, outdata[1], '0.3')
    #plt.legend[handles=["
    plt.ylabel("Value")
    plt.xlabel("Time")
    pylab.show()

if __name__ == "__main__":

    main()
