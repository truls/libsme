from PIL import Image
import itertools
import functools

from matplotlib import pyplot as plt

from sme import SME, Network, SimulationProcess, extends, ExternalBus


def set_tuple_el(tup, n, val):
    l = list(tup)
    l[n] = val
    return tuple(l)


def read_img(f):
    i = Image.open(f)
    return list(map(lambda x: (x[0], x[1], x[2], 0)
                    if len(x) != 2 else x, i.getdata()))


class ImageInput(SimulationProcess):
    def setup(self, ins, outs, images=None):
        self.map_outs(outs, "data")
        if images is None:
            images = ["image1.png", "image2.jpg", "image3.png"]
        images = list(map(lambda x: list(map(lambda y: set_tuple_el(y, 3, False),
                                             read_img(x))), images))
        for i in images:
            i[-1] = set_tuple_el(i[-1], 3, True)
        images = itertools.chain(*images)
        #print(images)
        self.pixel_gen = iter(images)

    def run(self):
        try:
            #print(next(self.pixel_gen))
            r, g, b, last = next(self.pixel_gen)
        except StopIteration:
            self.data["last_pixel"] = False
            self.data["valid"] = False
        else:
            #print("Sending values ", r, g, b, last)
            self.data["R"] = r
            self.data["G"] = g
            self.data["B"] = b
            self.data["last_pixel"] = last
            self.data["valid"] = True


class CollectResults(SimulationProcess):
    def setup(self, ins, outs, res):
        self.map_ins(ins, "result")
        self.res = res

    def run(self):
        # print("Got result ", self.result["low"], self.result["med"],
        #       self.result["high"])
        if self.result["valid"]:
            self.res.append((self.result["low"], self.result["med"],
                                self.result["high"]))

@extends("collector.sme")
class ColorBin(Network):
    def wire(self, result):
        # print("Wire: ", args, kwargs)
        # result = args[0]
        img_out_data = ExternalBus("image_input")
        result_bus = ExternalBus("bin_count_out")
        self.add(img_out_data)
        self.add(result_bus)

        input_gen = ImageInput("ImageInput", [], [img_out_data])
        result_collect = CollectResults("CollectResults", [result_bus], [], result)
        self.add(input_gen)
        self.add(result_collect)


if __name__ == "__main__":
    sme = SME()
    result = []
    sme.network = ColorBin("", "ColorBin", result)
    sme.network.clock(352686)

    if len(result) > 0:
        result = functools.reduce(lambda x, y: (x[0] + y[0], x[1] + y[1],
                                                x[2] + y[2]), result)
        print(result)
        plt.plot(result)
        #plt.show()
    else:
        print("empty result")
