from sme import SME, Network, SimulationProcess, ExternalBus, extends


class Id(SimulationProcess):
    def setup(self, ins, outs, result):
        self.map_outs(outs, "out")
        self.map_ins(ins, "inp")

    def run(self):
        #print("Got val", self.out["val"])
        result[0] = self.out["val"]
        self.out["val"] = self.inp["val"]
        self.out["valid"] = True


@extends("addone.sme")
class AddOne(Network):
    def wire(self, result):
        plus_out = ExternalBus("plusout")
        id_out = ExternalBus("idout")

        p = Id("Id", [plus_out], [id_out], result)

        self.add(plus_out)
        self.add(id_out)
        self.add(p)


if __name__ == "__main__":
    sme = SME()
    result = [0]
    sme.network = AddOne("", "AddOne", result)
    sme.network.clock(100)
    print("Final result was ", result[0])
