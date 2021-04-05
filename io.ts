import { IO } from "monet"
import { Collection, Range, Seq } from "immutable"

type Random = IO<number>
export const random: Random = IO(Math.random)

type Amplify = (min: number, max: number) => (x: number) => number
export const amplify: Amplify = (min, max) => x => {
    min = Math.ceil(min)
    max = Math.floor(max)
    return Math.floor(x * (max - min + 1)) + min
}

type RandRange = (min: number, max: number) => IO<number>
export const randRange: RandRange = (min, max) => random.map(amplify(min, max))

type Choice = <V>(items: Seq.Indexed<V>) => IO<V>
export const choice: Choice = items => randRange(0, items.size - 1).map(i => items.get(i))

type Choices = <V>(items: Seq.Indexed<V>) => (n: number) => IO<Seq<number, V>>
export const choices: Choices = items => n => sequenceIO(Range(0, n).map(() => choice(items)))

type SequenceIO = <T>(actions: Seq.Indexed<IO<T>>) => IO<Seq.Indexed<T>>
const sequenceIO: SequenceIO = actions => IO(() => actions.map(action => action.run()))

type Sample = (randomGenerator: IO<string>) => (n: number) => IO<Collection.Keyed<string, number>>
export const sample: Sample = randomGenerator => n => (
    sequenceIO(Range(0, n).map(() => randomGenerator))
    .map(
        results => (
            results
            .groupBy(x => x)
            .map(group => group.count())
        )
    )
);

const sum = (seq: Seq<any, number>) => seq.reduce((x,y) => x + y, 0)
export const sumOfIndependent = (actions: IO<Seq<any, number>>) => (
    actions.map(sum)
)

type DependentOn = <T>(a: IO<T>) => <V>(fn: (val: T) => IO<V>) => IO<V>
export const dependentOn: DependentOn = a => fn => a.flatMap(fn)

const moves = Seq(["rock", "paper", "scissors"] as const)
export const rockPaperScissorsGame = choice(moves)

export const dice = Range(1, 7)
export const diceThrow = choice(dice)
export const nDiceThrows = choices(dice)
export const sumOfNDiceThrows = (x: number) => sumOfIndependent(nDiceThrows(x))
export const diceGame = dependentOn(diceThrow)(sumOfNDiceThrows)
