import jsc from "jsverify"
import * as random from "./random"
import { List, Range } from "immutable";

const generators = {
  inOrderPairs: jsc.suchthat(
    jsc.pair(jsc.nat, jsc.nat),
    (pair) => pair[0] < pair[1]
  ),
  zeroToOne: jsc.suchthat(
    jsc.number,
    x => x >= 0 && x < 1
  ),
  positiveInt: jsc.suchthat(
    jsc.integer,
    x => x > 0
  )
}


describe("amplify", () => {
  test(
    "always returns numbers in range",
    () => {
      jsc.assert(
        jsc.forall(
          generators.inOrderPairs,
          generators.zeroToOne,
          (pair: [number, number], x: number) => {
            const [min, max] = pair
            const result = random.amplify(min, max)(x)
            return result >= min && result <= max
          }
        )
      )
    }
  );
});

describe("randRange", () => {
  test(
    "all values in range are represented, not inclusive of max",
    () => {
      jsc.assert(
        jsc.forall(
          generators.inOrderPairs,
          (pair: [number, number]) => {
            const [min, max] = pair;
            const generator = random.randRange(min, max)
            const sampling = random.sample(generator)(5000)
            const results = sampling.run()
            const expectedKeys = List(Range(min, max));
            return List(results.keys()).sort().equals(expectedKeys)
          }
        )
      )
    }
  )
})
