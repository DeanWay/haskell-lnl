import { IO } from "monet"
import { Collection, Range } from "immutable"

type Random = IO<number>
export const random: Random = IO(Math.random)

type SumOfRandoms = (min: number, max: number) => IO<Number>
export const sumOfRandoms: SumOfRandoms = (min, max) => {
    const dice = randRange(min, max)
    return dice.ap(dice.map(x => y => x + y))
}

type Amplify = (min: number, max: number) => (x: number) => number
export const amplify: Amplify = (min, max) => x => {
    min = Math.ceil(min)
    max = Math.floor(max)
    return Math.floor(x * (max - min + 1)) + min
}

type RandRange = (min: number, max: number) => IO<number>
export const randRange: RandRange = (min, max) => random.map(amplify(min, max))

type Choose = <T>(items: ReadonlyArray<T>) => IO<T>
export const choose: Choose = items => randRange(0, items.length - 1).map(i => items[i])

type Sample = (randomGenerator: IO<string>) => (n: number) => IO<Collection.Keyed<string, number>>
export const sample: Sample = randomGenerator => n => (
    IO(() => (
        Range(0, n)
        .map(() => randomGenerator.run())
        .groupBy(x => x)
        .map(x => x.count())
    ))
);

const moves = ["rock", "paper", "scissors"] as const
type Move = typeof moves[number]
type Game = IO<Move>
export const game: Game = choose(moves)

// const show = (x: any) => JSON.stringify(x)
// const myRandRange = randRange(0, 50)
// const mySampler = sample(myRandRange.map(show))
// const print = (x: any) => IO(() => console.log(x))
// mySampler(10000000).bind(print).run()
