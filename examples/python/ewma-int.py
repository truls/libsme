#!/usr/bin/env python3
from sme import External, Network, Function, Bus, SME
from sme import ConstrainedTypes as t
#from sme import Types as t
#from baseavg import *
import numpy as np
from matplotlib import pyplot as plt
import pylab
import sys

class Source(External):
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
        self.out["valid"] = 1


class Logger(External):
    def setup(self, ins, outs, count, results):
        self.map_ins(ins, "shortd", "longd")
        self.results = results
        self.curpos = 0

    def run(self):
        #print(self.__dict__["data"])
        if self.shortd["valid"]:
            print(self.results.shape)
            self.results[0][self.curpos] = self.shortd["val"]
        if self.longd["valid"]:
            print(self.results.shape)
            self.results[1][self.curpos] = self.longd["val"]
        self.curpos += 1


class Calc(Function):
    def run(self):
        if self.inbus["valid"] == 1:
            #self.prev = (self.inbus["val"] >> self.d) + (self.prev >> self.d) * (self.d_shift - 1)
            self.prev = (self.inbus["val"] >> self.d) + (self.prev >> self.d) * ((1 << self.d) - self.sub)
            self.outbus["val"] = self.prev
            self.outbus["valid"] = 1
        else:
            self.outbus["valid"] = 0

    def setup(self, ins, outs, decay: t.i3):
        self.map_ins(ins, "inbus")
        self.map_outs(outs, "outbus")
        self.d = decay
        #self.decl_var("d", decay, smeint(10))
        #self.d_shift = 1 << decay
        self.sub = 1 # Type: t.u1
        self.prev = 0 # Type: t.i16

class EWMA(Network):
    def wire(self, indata, outdata, *args, **kwargs):
        count = len(indata)


        bus1 = Bus("bus1", [t.i16("val"), t.boolean("valid")], int)
        bus1["valid"] = 0
        self.tell(bus1)
        short = Bus("short", [t.i16("val"), t.boolean("valid")], int)
        self.tell(short)
        short["valid"] = 0
        longb = Bus("longb", [t.i16("val"), t.boolean("valid")], int)
        self.tell(longb)
        longb["valid"] = 0

        logger = Logger("Logger", [short, longb], [], count, outdata)
        self.tell(logger)
        source = Source("Source", [], [bus1], indata, count)
        self.tell(source)
        decay = 2
        shortc = Calc("shortCalc", [bus1], [short], decay)
        self.tell(shortc)
        decay = 3
        longc = Calc("longCalc", [bus1], [longb], decay)
        self.tell(longc)


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
    sme.network = EWMA("EWMA", indata, outdata)
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
