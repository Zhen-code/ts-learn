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
