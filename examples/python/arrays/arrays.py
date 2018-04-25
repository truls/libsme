from sme import SME, Network, SimulationProcess, ExternalBus, extends


class Reader(SimulationProcess):
    def setup(self, ins, outs, result):
        self.map_ins(ins, "inp")
        self.n = 0

    def run(self):
        #if self.inp["valid"]:
        print("Iteration {} got value {}".format(self.n, self.inp["val"]))
        self.n += 1


class Control(SimulationProcess):
    def setup(self, ins, out):
        self.map_outs(out, "out")
        self.n = 0
        self.LOAD, self.STORE = (1, 0)
        self.state = self.STORE

    def run(self):
        self.out["valid"] = True
        self.out["mode"] = self.state
        self.out["pos"] = self.n % 10
        self.out["val"] = self.n
        if self.n % 10 == 0:
            if self.state == self.STORE:
                self.state = self.LOAD
            else:
                self.state = self.STORE
        self.n += 1

@extends("arrays.sme", ["-t", "trace.csv", "-f"])
class Arrays(Network):
    def wire(self, result):
        arr_out = ExternalBus("arr_out")
        control = ExternalBus("control")

        p = Reader("Id", [arr_out], [], result)
        c = Control("Control", [], [control])

        self.add(arr_out)
        self.add(control)
        self.add(p)
        self.add(c)


if __name__ == "__main__":
    sme = SME()
    result = [0]
    sme.network = Arrays("", "AddOne", result)
    sme.network.clock(100)
