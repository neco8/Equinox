import { StorageQuerySDL } from "./typescript/storage";

export type Category = {
  id: string;
  title: string;
};

export type BreathingMethod = {
  id: string;
  name: string;
  "category-id": string;
  "created-at": number;
  inhale: number;
  "inhale-hold": number;
  exhale: number;
  "exhale-hold": number;
};

export type Session = {
  id: string;
  "breathing-method-id": string;
  "breathing-method-name": string;
  inhale: number;
  "inhale-hold": number;
  exhale: number;
  "exhale-hold": number;
  duration: number;
  "created-at": number;
};

// Query Result Types

// [ ] TODO: このような型が抽象的なクエリリザルトはいらない。CategoryやBreathingMethod等に合わせて、それぞれ愚直にリザルトを作成する
// [ ] TODO: ポートの命名規則がほしい
// [ ] TODO: ポートで、elmで設定したポートがすべてd.tsに存在するか確認する機構がほしい

export type Entity = "category" | "breathing-method" | "session";

// - [ ] TODO: これ多分いらない
export type QueryListResult =
  | {
      type: "category-list";
      data: unknown[];
    }
  | {
      type: "breathing-method-list";
      data: unknown[];
    }
  | {
      type: "session-list";
      data: unknown[];
    };

export type QuerySingleResult = {
  type: "entity-single";
  entity: Entity;
  data: Category | BreathingMethod | Session;
};

export type QueryResult = QueryListResult | QuerySingleResult;

// Errorタイプとかも、定義自身に確固たる理由がなく、ただ起こりそうだからぐらい。デコードはelmしか知ってはいけない知識だからここにあるのはおかしいよね。
// Error Types
export type QueryError =
  | { type: "NotFound" }
  | { type: "DecodingError"; message: string }
  | { type: "QueryError"; message: string }
  | { type: "UnknownError"; message: string };

// これはまああり。
// Save Result Type
export type SaveResult =
  | { type: "success" }
  | { type: "error"; message: string };

type Flags = {
  now: number;
  environment: "development" | "production" | "test";
};

// Command and Subscription Types
export type Cmd<T> = {
  subscribe: (fn: (value: T) => void) => void;
};

export type Sub<T> = {
  send: (value: T) => void;
};

// ポート系怪しい。
// Port Interfaces
export interface Ports {
  queryStorage: Cmd<StorageQuerySDL>;
  receiveQueryResult: Sub<QueryResult>;
  receiveQueryError: Sub<QueryError>;
  // save系は、結構ありだと思う。でも、複数保存のユースケースは要らない？今のところは要らないからきっとYAGNIなんだろうが、不安だ。
  saveBreathingMethod: Cmd<BreathingMethod>;
  saveCategory: Cmd<Category>;
  saveSession: Cmd<Session>;
  // [ ] TODO: saveResultについても実装する
  // UUID
  generateUuid: Cmd<string>; // callbackを識別するtag
  receiveUuid: Sub<[string, string]>; // [tag, uuid] tagによって識別されるcallbackにuuidを渡す
  // 削除
  deleteBreathingMethod: Cmd<string>;
  receiveDeleteBreathingMethodResult: Sub<boolean>;
  // 効果音
  playSound: Cmd<string>;
}

// Elm App Interface
export namespace Elm {
  export namespace Main {
    export interface App {
      ports: Ports;
    }
    export function init(param: {
      node: HTMLElement;
      flags: Flags;
    }): Elm.Main.App;
  }
}
