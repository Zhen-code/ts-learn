/**
 * 接口遍历keyof，联合类型遍历in
 */
 type MyPick<T, K extends keyof T> = {
    [P in K]: T[P]
  }
/**
 * 设置接口属性只读
 */
  type MyReadonly<T> = {
    readonly [P in keyof T]: T[P]
  }

  /**
   * 遍历数组值
   * in T[number] T：Array
   */
  type TupleToObject<T extends readonly (string|number|symbol)[]> = {
    [P in T[number]]: P
  }

/**
 * 是否某种值类型
 * T extends []
 */
  type First<T extends any[]> =  T extends [] ? never: T[0]
/**
 * 只读类型数组 readonly any[]
 */
  type Length<T extends readonly any[]> = T['length']
/**
 * 联合类型遍历另一联合类型
 */
  type MyExclude<T, U> = T extends U ? never:T
/**
 * Promise类型使用
 */
  type thenAble<T> = {
    then: (onfulfilled: (arg:T)=>any) => any
  }
  type MyAwaited<T extends Promise<any>|thenAble<any>> = T extends Promise<infer P> ? P extends Promise<any> ? MyAwaited<P> : P : T extends thenAble<infer U> ? U: false
/**
 * IF
 */
  type If<C extends boolean, T, F> = C extends true ? T : F
/**
 * 类型数组元素包含判断
 */
  type Includes<T extends readonly any[], U> = T extends [infer first, ...infer rest] ? Equal<first, U> extends true ? true : Includes<rest, U> : false
/**
 * 获取函数参数类型
 */
type MyParameters<T extends (...args: any[]) => any> = T extends (...args: infer S) => any ? S : []
/**
 * 函数返回值类型
 */
type MyReturnType<T extends Function> = T extends (...args:any[]) => infer P ? P : void
/**
 * 联合类型是否属于接口类型的key  K extends keyof T(T联合)
 * 接口类型必须要有key:value
 * 遍历接口类型的key声明变量使用as
 */
type MyOmit<T, K extends keyof T> = {
    [P in keyof T as P extends K ? never:P]: T[P]
}
/**
 * 设置接口部分所需的属性为只读
 */
type MyReadonly2<T, K extends keyof T = keyof T> = {
    readonly [P in keyof T as P extends K ? P: never ]: T[P]
  } & {
    [P in keyof T as P extends K ? never: P ]: T[P]
}
/**
 * 判断是否对象或数组  extends Record<any,any>
 */
type DeepReadonly<T> = {
    readonly [P in keyof T]: T[P] extends Record<any,any> ? T[P] extends Function ? T[P] : DeepReadonly<T[P]> : T[P]
  }
/**
 * 数组转联合类型
 */
type TupleToUnion<T extends any[]> = T[number]

/**
 * 判断key是否存在 K extends keyof T ? never : K
 * 删除替换新属性：Omit<T, K> & Record<K, V>
 */
type Chainable<T = {}> = {
    option: <K extends string, V>(
        key: K extends keyof T ? never : K,
        value: V
      ) => Chainable<Omit<T, K> & Record<K, V>>;
    get: () => T;
};
/**
 * 数组最后一个元素
 */
type Last<T extends any[]> = [any, ...P][[P['length']]]
/**
 * 数组pop
 * 数组类型转换时使用extends获取新类型
 */
type Pop<T extends any[]> = T extends [...infer P, any] ? P: T
/**
 * promise.all实现
 * 返回数组类型： { [K in keyof T]: T[K] }
 */
declare function PromiseAll<T extends readonly any[]>(values:[...T]):Promise<{ [K in keyof T]: T[K] extends Promise<infer R> ? R : T[K] extends number ? T[K] : number }>
/**
 * 查找满足某个类型的联合类型
*/
type LookUp<U, T> = U extends {type:T} ? U:never
/**
 * 去除字符串左侧空格
 */
type Space = ' ' | '\n' | '\t'
type TrimLeft<S extends string> = S extends `${Space}${infer R}` ? TrimLeft<R> : S
/**
 * 去除字符串空格
 */
type Space = ' ' | '\n' | '\t'
type Trim<S extends string> = S extends `${Space}${infer T}` | `${infer T}${Space}` ? Trim<T> : S;
/**
 * str replace From to To
 */
type Replace<S extends string, From extends string, To extends string> =
From extends '' ? S
: S extends `${infer F}${From}${infer L}` ? `${F}${To}${L}` : S
/**
 * str replaceAll From to To
 */
type ReplaceAll<S extends string, From extends string, To extends string> = From extends '' ? S : S extends `${infer F}${From}${infer L}` ? `${F}${To}${ReplaceAll<L, From, To>}` : S;
/**
 * append argument
 */
type AppendArgument<Fn extends Function, A extends any> = Fn extends (...args: infer R) => infer T ? (...args: [...R, A]) => T : never
/**
 * 类型排列
 */
type Permutation<T, U = T> = [U] extends [never] ? [] : (T extends U  ? [T, ...Permutation<Exclude<U, T>>] : [])
/**
 * 字符串长度
 * 设置初始值  T extends string[] = []
 * 字符串类型提取：S extends `${infer F}${infer R}`
 */
type LengthOfString<S extends string, T extends string[] = []> = S extends `${infer F}${infer R}` ? LengthOfString<R, [F,...T]> : T['length']
/**
 * 数组拍平
*/
type Flatten<T extends any[], U extends any[] = []> = T extends [infer F, ...infer R] ?
F extends any[] ? Flatten<[...F, ...R], U> : Flatten<[...R], [...U, F]>  : U;
/**
 * 对象添加属性
 */
type AppendToObject<T, U extends keyof any, V> = {
    [P in keyof T | U]: P extends keyof T ? T[P]: V
}
/**
 * 绝对值
*/
type Absolute<T extends number | string | bigint> = `${T}` extends `-${infer R2}` ? R2 : `${T}`
/**
 * 字符串改成联合类型
 */
type StringToUnion<T extends string> = T extends `${infer R1}${infer R2}` ? R1 | StringToUnion<R2> : never
/**
 * 对象类型合并
 * 属性是否在对象类型中：K extends keyof S
 */
type Merge<F extends object, S extends object> = {
    [K in keyof F | keyof S]: K extends keyof S ? S[K] : K extends keyof F ? F[K] : never
}
/**
 * Replace the `camelCase` or `PascalCase` string with `kebab-case`.
 */
type KebabCase<S extends string> = S extends `${infer R1}${infer R2}` ? R2 extends Uncapitalize<R2> ? `${Uncapitalize<R1>}${KebabCase<R2>}` : `${Uncapitalize<R1>}-${KebabCase<R2>}` : S
/**
 * Get an Object that is the difference between O & O1
 */
type Diff<O, O1> = Omit<O & O1, keyof (O|O1)>
/**
 * Implement Python liked any function in the type system. A type takes the Array and returns true if any element of the Array is true. If the Array is empty, return false.
 */
type P = 0 | '' | false | [] | {[key:string]:never} | undefined | null
type AnyOf<T extends readonly any[]> = T[number] extends P ? false:true
/**
 * Implement a type IsNever, which takes input type T. If the type of resolves to never, return true, otherwise false.
 */
type IsNever<T> = [T] extends [never] ? true:false
/**
 * 判断是否联合类型
 */
type IsUnion<T, U extends T = T> = (T extends T ? U extends T ? true:unknown: never) extends true ? false:true
/**
 * U中的T属性Y中同属性类型
 */
type ReplaceKeys<U, T, Y> = {
    [K in keyof U]: K extends T ? K extends keyof Y ? Y[K]:never : U[K]
  }
/**
 * Remove Index Signature
 */
type RemoveIndexSignature<T, P = PropertyKey> = {
    [K in keyof T as P extends K ? never: K extends P ? K:never]: T[K]
  }
/**
 * type PString2 = "+85%"
 * type R2 = PercentageParser<PString2> // expected ["+", "85", "%"]
 */
type Prefix<T> = T extends '+' | '-' ? T : never;
type Suffix<T> = T extends `${infer R}${'%'}` ? [R, '%'] : [T, ''];
type PercentageParser<A extends string> = A extends `${Prefix<infer L>}${infer R}` ? [L, ...Suffix<R>] : ['', ...Suffix<A>];

/**
 * 去除字符串空格
 */
type DropChar<S, C extends string> = S extends `${infer L}${C}${infer R}` ? DropChar<`${L}${R}`, C> : S
/**
 * 判断是否以U开头
 */
type StartsWith<T extends string, U extends string> = T extends `${U}${infer P}` ? true:false
/**
 * 两个对象遍历类型进行合并后需要使用对象类型再次遍历后返回
 * K extends keyof T = any：设置K约束为T中的属性类型，若K为空则与any类型一致
 */
type IntersectionToObj<T> = {
  [P in keyof T]: T[P]
}
type PartialByKeys<T, K extends keyof T = any> = IntersectionToObj<{
 [P in keyof T as P extends K ? P:never]?: T[P]
} & {
[P in Exclude<keyof T, K>]: T[P]
}>

// 设置BEM格式
// type cases = [
//     Expect<Equal<BEM<'btn', ['price'], []>, 'btn__price'>>,
//     Expect<Equal<BEM<'btn', ['price'], ['warning', 'success']>, 'btn__price--warning' | 'btn__price--success' >>,
//     Expect<Equal<BEM<'btn', [], ['small', 'medium', 'large']>, 'btn--small' | 'btn--medium' | 'btn--large' >>,
// ]
type isNever<T> = T extends never ? true:false
type isUnion<T> = isNever<T> extends true ? "" : T
type BEM<B extends string, E extends string[], M extends string[]> = `${B}${isUnion<`__${E[number]}`>}${isUnion<`--${M[number]}`>}`

// inorder binary tree
interface TreeNode {
    val: number
    left: TreeNode | null
    right: TreeNode | null
}
type InorderTraversal<T extends TreeNode | null> = [T] extends [TreeNode] ? [...InorderTraversal<T['left']>, T['val'], ...InorderTraversal<T['right']>]:[]

/**
 *  Flip<{ a: "x", b: "y", c: "z" }>; // {x: 'a', y: 'b', z: 'c'}
 * */
type Flip<T> = {
    [K in keyof T as `${T[K]}`]: K
}
/*
* Fibonacci:类型声明非获取真实值
* */
type Fibonacci<T extends number, N extends number[] = [1], Prev extends any[] = [], Current extends any[] = [1]> =
    T extends N['length'] ? Current['length']: Fibonacci<T, [...N, 1], Current, [...Prev, ...Current]>

/**
 * Expect<Equal<Zip<[], []>, []>>,
  Expect<Equal<Zip<[1, 2], [true, false]>, [[1, true], [2, false]]>>,
  Expect<Equal<Zip<[1, 2, 3], ['1', '2', '3']>, [[1, '1'], [2, '2'], [3, '3']]>>,
  Expect<Equal<Zip<[], [1, 2, 3]>, []>>,
  Expect<Equal<Zip<[[1, 2]], [3]>, [[[1, 2], 3]]>>,
  Expect<Equal<Zip<[1, 2, 3], ['1', '2']>, [[1, '1'], [2, '2']]>>
  变量声明：infer    数组类型判断：Arr extends [infer A, ...infer B]
 */
type Zip<T, U, R extends any[] = []> = T extends [infer R1, ...infer R2] ? U extends [infer R3, ...infer R4] ? Zip<R2,R4,[...R,[R1, R3]]> : R : R

/**
 * 元组判断：长度固定且只读
 * 判断是否never [T] extends [never]
 * number只能继承number类型，不能继承具体数字
 */
type IsTuple<T> =
  [T] extends [never] ? false:
  T extends readonly any[]?
    number extends T['length']?false:true
  :false


  /**
   * Expect<Equal<Chunk<[], 1>, []>>,
  Expect<Equal<Chunk<[1, 2, 3], 1>, [[1], [2], [3]]>>,
   */
  type Chunk<
  T extends any[], // 原始数组类型
  N extends number, // 每个子数组的长度
  Swap extends any[] = [] // 用于存储当前子数组的变量
> =
  Swap['length'] extends N // 如果当前子数组的长度已经达到了 N
    ? [Swap, ...Chunk<T, N>] // 将当前子数组加入结果数组中，继续拆分剩余部分
    : T extends [infer K, ...infer L] // 如果原始数组还有剩余部分
      ? Chunk<L, N, [...Swap, K]> // 将下一个元素加入当前子数组中，继续拆分剩余部分
      : Swap extends [] ? Swap : [Swap] // 如果原始数组已经被拆分完毕，则返回最后一个子数组

/**
 * Expect<Equal<Fill<[1, 2, 3], true>, [true, true, true]>>,
  Expect<Equal<Fill<[1, 2, 3], true, 0, 1>, [true, 2, 3]>>,
  Expect<Equal<Fill<[1, 2, 3], true, 1, 3>, [1, true, true]>>,
  Expect<Equal<Fill<[1, 2, 3], true, 10, 0>, [1, 2, 3]>>,
  Expect<Equal<Fill<[1, 2, 3], true, 10, 20>, [1, 2, 3]>>,
 * 
 */
      type Fill<
      T extends unknown[],
      N,
      Start extends number = 0,
      End extends number = T['length'],
      Count extends any[] = [],
      Flag extends boolean = Count['length'] extends Start ? true:false
    > = Count['length'] extends End ? T : T extends [infer F, ...infer R] ?
    Flag extends false ? [F, ...Fill<R, N, Start, End, [...Count, 0]>]: [N, ...Fill<R, N, Start, End, [...Count, 0], Flag>] : T


    /**
     * 去除右边空格字符
     */
    type TrimRight<S extends string> = S extends `${infer L}${' ' | '\n' | '\t'}` ? TrimRight<L>:S


    /**
     * Expect<Equal<Trunc<-5.1>, '-5'>>,
  Expect<Equal<Trunc<'.3'>, '0'>>,
  Expect<Equal<Trunc<'1.234'>, '1'>>,
     */
    type Trunc<U extends number|string, T = `${U}`> = T extends `${infer L}.${infer R}` ? L extends "" ? '0' : L : T

/**
 * Expect<Equal<Join<['2', '2', '2'], 1>, '21212'>>,
   Expect<Equal<Join<['o'], 'u'>, 'o'>>,
 */
type Join<T extends unknown[], U extends string|number, R extends any = ''> = T extends [infer F, ...infer Rest] ? Rest extends [] ? 
Join<Rest, U, `${R}${F}`> : Join<Rest, U, `${R}${F}${U}`> : `${R}`

/**
 *  Expect<Equal<LastIndexOf<[0, 0, 0], 2>, -1>>,
  Expect<Equal<LastIndexOf<[string, 2, number, 'a', number, 1], number>, 4>>,
  isEqual:判断any类型是否相等
 */
type isEqual<T, U> = U extends T ? T extends U ? true : false : false
type LastIndexOf<T extends any[], U> = T extends [...infer F, infer Rest] ? isEqual<Rest, U> extends true ? F['length'] : LastIndexOf<F, U> : -1

/**
 * Equal内置解决any与unknown相等问题
 */
type Arrhas<T extends any[] = [], U extends any = any> = T extends [infer F, ...infer Rest] ? Equal<F, U> extends true ? true : Arrhas<Rest, U>:false  
type Unique<T extends any[], U extends any[] = []> = T extends [infer F, ...infer Rest] ? Arrhas<U, F> extends false ? Unique<Rest, [...U, F]> : Unique<Rest, U> : U

/**
  Expect<Equal<MapTypes<{ name: string, date: Date }, { mapFrom: string, mapTo: boolean } | { mapFrom: Date, mapTo: string }>, { name: boolean, date: string }>>,
 * 排除联合类型使用extends判断，不符合则设为never
 */
type MapTypes<T, R extends {mapFrom: any, mapTo: any}> = {
  [P in keyof T]: T[P] extends R['mapFrom'] ? R extends {mapFrom:T[P]} ? R['mapTo']:never:T[P]
}

/**
 * type result = NumberRange<2 , 9> //  | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 */
type A<T extends number, U extends any[] = []> = U['length'] extends T ? U: A<T,[...U,1]>
type NumberRange<
  L extends number,
  H extends number,
  Idx extends 1[] = L extends 0 ? [] : A<L>,
  Res = never
> = 
Idx['length'] extends H ? H|Res : NumberRange<L,H,[...Idx,1],Idx['length']|Res>

/**
 * type cases = [
  Expect<Equal<Combination<['foo', 'bar', 'baz']>, 'foo' | 'bar' | 'baz' | 'foo bar' | 'foo bar baz' | 'foo baz' | 'foo baz bar' | 'bar foo' | 'bar foo baz' | 'bar baz' | 'bar baz foo' | 'baz foo' | 'baz foo bar' | 'baz bar' | 'baz bar foo'>>,
]
 */
type Combination<T extends string[], All extends string = T[number], Item extends string = All> = 
Item extends string ? Item | `${Item} ${Combination<[],Exclude<All, Item>>}` : never  