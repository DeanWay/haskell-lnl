import { IO } from "monet"
import { Range, List, Seq, Map } from "immutable"

type Amplify = (min: number, max: number) => (x: number) => number
export const amplify: Amplify = (min, max) => x => {
    min = Math.ceil(min)
    max = Math.floor(max)
    return Math.floor(x * (max - min + 1)) + min
}


type Random = IO<number>
export const random: Random = IO(Math.random)

type RandRange = (min: number, max: number) => IO<number>
export const randRange: RandRange = (min, max) => random.map(amplify(min, max - 1))

type Choice = <A>(items: List<A>) => IO<A>
export const choice: Choice = items => randRange(0, items.size).map(i => items.get(i)!)

type Choices = <A>(items: List<A>) => (n: number) => IO<List<A>>
export const choices: Choices = items => n => repeat(n)(choice(items))

type Sample = <A>(randomGenerator: IO<A>) => (n: number) => IO<Map<A, number>>
export const sample: Sample = randomGenerator => n => (
    repeat(n)(randomGenerator)
    .map(
        results => (
            results
            .groupBy(x => x)
            .map(group => group.count())
            .toMap()
        )
    )
);

type DependentOn = <T>(a: IO<T>) => <V>(fn: (val: T) => IO<V>) => IO<V>
export const dependentOn: DependentOn = a => fn => a.flatMap(fn)

const moves = List(["rock", "paper", "scissors"] as const)
export const rockPaperScissorsGame = choice(moves)

export const dice = Range(1, 7).toList()
export const diceThrow = choice(dice)
export const nDiceThrows = choices(dice)
export const sumOfNDiceThrows = (x: number) => nDiceThrows(x).map(sum)
export const diceGame = dependentOn(diceThrow)(sumOfNDiceThrows)


type SequenceIO = <K, V>(actions: Seq<K, IO<V>>) => IO<Seq<K, V>>
const sequenceIO: SequenceIO = actions => IO(() => actions.map(action => action.run()))

type Repeat = (n: number) => <T>(action: IO<T>) => IO<List<T>>
const repeat: Repeat = n => action => (
    sequenceIO(
        Range(0, n).map(() => action)
    ).map(seq => seq.toList())
)

interface Foldable<A> {
    reduce<B>(reducer: (accum: B, item: A) => B, initial?: B): B
}
const sum = (list: Foldable<number>) => list.reduce((x, y) => x + y, 0)
