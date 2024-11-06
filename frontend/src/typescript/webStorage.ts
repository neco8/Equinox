import { NotFoundError, Storage, StorageKey, StorageQuerySDL } from "./storage";
import { QueryListResult, QuerySingleResult, SaveResult } from "../Main.elm";

// 将来的に、パスで値を返すというシンプルな形にしないといけないのかも。
// でも、今はこれで動くのを確認しよう。
class WebTestStorage implements Storage {
  private getData(key: StorageKey) {
    const rawData = localStorage.getItem(key);
    if (rawData === null) throw new NotFoundError();

    const data = JSON.parse(rawData);
    return data;
  }

  private filterFunc(data: { [key: string]: unknown }[], q: StorageQuerySDL) {
    if (q.conditions !== undefined) {
      const conditions = q.conditions;
      return data.filter((item) => {
        return conditions.every(({ field, operator, value }) => {
          switch (operator) {
            case "eq":
              return item[field] === value;
            case "gt":
              try {
                return (item[field] as any) > value;
              } catch (error) {
                return false;
              }
            case "lt":
              try {
                return (item[field] as any) < value;
              } catch (error) {
                return false;
              }
            case "gte":
              try {
                return (item[field] as any) >= value;
              } catch (error) {
                return false;
              }
            case "lte":
              try {
                return (item[field] as any) <= value;
              } catch (error) {
                return false;
              }
          }
        });
      });
    } else {
      return data;
    }
  }

  async save(key: StorageKey, value: unknown): Promise<SaveResult> {
    localStorage.setItem(key, JSON.stringify(value));
    return {
      type: "success",
    };
  }

  async queryAll(
    key: StorageKey,
    q: StorageQuerySDL
  ): Promise<QueryListResult> {
    const data = Object.values(this.getData(key));

    switch (key) {
      case "EQUINOX_breathing-methods":
        return {
          type: "breathing-method-list",
          data: this.filterFunc(data as any, q),
        };
      case "EQUINOX_categories":
        return {
          type: "category-list",
          data: this.filterFunc(data as any, q),
        };
      case "EQUINOX_sessions":
        return {
          type: "session-list",
          data: this.filterFunc(data as any, q),
        };
    }
  }

  async queryOne(
    key: StorageKey,
    query: StorageQuerySDL
  ): Promise<QuerySingleResult> {
    // [ ] TODO: 明らかにここのようなqueryOneなど、使い勝手がおかしくなっている。最初から何処かが狂っている
    switch (key) {
      case "EQUINOX_breathing-methods":
      case "EQUINOX_categories":
      case "EQUINOX_sessions":
        const data = this.getData(key);
        if (Object.entries(data).length === 0) throw new NotFoundError();
        const result = this.filterFunc(data, query);
        return {
          type: "entity-single",
          entity: (() => {
            switch (key) {
              case "EQUINOX_breathing-methods":
                return "breathing-method";
              case "EQUINOX_categories":
                return "category";
              case "EQUINOX_sessions":
                return "session";
            }
          })(),
          data: result[0],
        };
    }
  }
}

export { WebTestStorage };
