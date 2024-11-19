import { QueryListResult, QuerySingleResult, SaveResult } from "../Main.elm";

// 以下のエラークラスを送出すること。

export class NotFoundError extends Error {
  constructor() {
    super("Not Found");
    this.name = "NotFoundError";
  }
}

export class QueryError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "QueryError";
  }
}

export type StorageKey = "EQUINOX_breathing-methods" | "EQUINOX_categories" | "EQUINOX_sessions";

// SDL Query Types
export type Operator = "eq" | "gt" | "lt" | "gte" | "lte";

export interface Condition {
  field: string;
  operator: Operator;
  value: string | number | boolean;
}

export interface StorageQuerySDL {
  storageKey: StorageKey
  conditions: Condition[] | undefined;
}

// Storage Interface
export interface Storage {
  save(key: StorageKey, value: unknown): Promise<SaveResult>;
  queryAll(key: StorageKey, query: StorageQuerySDL): Promise<QueryListResult>;
  queryOne(key: StorageKey, query: StorageQuerySDL): Promise<QuerySingleResult>;

  delete(key: StorageKey, id: string): Promise<void>;
}
