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
