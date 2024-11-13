import "./styles/index.css";
import { Elm } from "./Main.elm";
import { WebTestStorage } from "./typescript/webStorage";
import type {
  QueryResult,
  Category,
  BreathingMethod,
  Session,
} from "./Main.elm";
import {
  NotFoundError,
  QueryError,
  StorageQuerySDL,
} from "./typescript/storage";
import { applyUuid } from "./typescript/uuid";

// ストレージインスタンスの初期化
const storage = new WebTestStorage();

// Elmアプリケーションの初期化
const elem = document.getElementById("main");
if (elem) {
  const app = applyUuid(
    Elm.Main.init({
      node: elem,
      flags: {
        now: Date.now(),
        environment: "development",
      },
    })
  );

  const queryStorage = async (query: StorageQuerySDL) => {
    try {
      // ここでqueryAllしか使ってない。だけど、大きな前進だ。
      const result: QueryResult = await storage.queryAll(
        query.storageKey,
        query
      );

      app.ports.receiveQueryResult.send(result);
    } catch (error) {
      if (error instanceof QueryError) {
        app.ports.receiveQueryError.send({
          type: "QueryError",
          message: error.message,
        });
      } else {
        app.ports.receiveQueryError.send({
          type: "UnknownError",
          message: error instanceof Error ? error.message : "Unknown error",
        });
      }
    }
  };

  // ストレージクエリの処理
  app.ports.queryStorage.subscribe(async (query: StorageQuerySDL) => {
    queryStorage(query);
  });

  // Save操作のポートハンドラー設定
  app.ports.saveBreathingMethod.subscribe(async (datum: BreathingMethod) => {
    try {
      const data = await storage.queryAll("EQUINOX_breathing-methods", {
        storageKey: "EQUINOX_breathing-methods",
        conditions: [],
      });
      switch (data.type) {
        case "breathing-method-list":
          await storage.save(
            "EQUINOX_breathing-methods",
            Object.fromEntries(
              ([...data.data, datum] as BreathingMethod[]).map((d) => [d.id, d])
            )
          );
          break;
        default:
          throw new Error("Unexpected error");
      }
    } catch (error) {
      if (error instanceof NotFoundError) {
        await storage.save("EQUINOX_breathing-methods", { [datum.id]: datum });
      }
    }

    queryStorage({
      storageKey: "EQUINOX_breathing-methods",
      conditions: [],
    });
  });

  app.ports.saveCategory.subscribe(async (datum: Category) => {
    try {
      const data = await storage.queryAll("EQUINOX_categories", {
        storageKey: "EQUINOX_categories",
        conditions: [],
      });
      switch (data.type) {
        case "category-list":
          console.log(
            Object.fromEntries(
              ([...data.data, datum] as Category[]).map((d) => [d.id, d])
            )
          );
          await storage.save(
            "EQUINOX_categories",
            Object.fromEntries(
              ([...data.data, datum] as Category[]).map((d) => [d.id, d])
            )
          );
          break;
        default:
          throw new Error("Unexpected error");
      }
    } catch (error) {
      if (error instanceof NotFoundError) {
        await storage.save("EQUINOX_categories", { [datum.id]: datum });
      }
    }

    queryStorage({
      storageKey: "EQUINOX_categories",
      conditions: [],
    });
  });

  app.ports.saveSession.subscribe(async (datum: Session) => {
    try {
      const data = await storage.queryAll("EQUINOX_sessions", {
        storageKey: "EQUINOX_sessions",
        conditions: [],
      });
      switch (data.type) {
        case "session-list":
          await storage.save(
            "EQUINOX_sessions",
            Object.fromEntries(
              ([...data.data, datum] as Session[]).map((d) => [d.id, d])
            )
          );
          break;
        default:
          throw new Error("Unexpected error");
      }
    } catch (error) {
      if (error instanceof NotFoundError) {
        await storage.save("EQUINOX_sessions", { [datum.id]: datum });
      }
    }

    queryStorage({
      storageKey: "EQUINOX_sessions",
      conditions: [],
    });
  });
}
