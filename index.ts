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
 */
declare function PromiseAll<T extends readonly any[]>(values:[...T]):Promise<{ [K in keyof T]: T[K] extends Promise<infer R> ? R : T[K] extends number ? T[K] : number }>


  
