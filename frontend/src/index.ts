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
  const app = applyUuid(Elm.Main.init({ node: elem, flags: Date.now() }));

  // ストレージクエリの処理
  app.ports.queryStorage.subscribe(async (query: StorageQuerySDL) => {
    try {
      // ここでqueryAllしか使ってない。だけど、大きな前進だ。
      const result: QueryResult = await storage.queryAll(
        query.storageKey,
        query
      );

      app.ports.receiveQueryResult.send(result);
    } catch (error) {
      if (error instanceof NotFoundError) {
        app.ports.receiveQueryError.send({ type: "NotFound" });
        return;
      } else if (error instanceof QueryError) {
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
  });

  // Save操作のポートハンドラー設定
  app.ports.saveBreathingMethod.subscribe(async (data: BreathingMethod) => {
    await storage.save("EQUINOX_breathing-methods", data);
  });

  app.ports.saveCategory.subscribe(async (data: Category) => {
    await storage.save("EQUINOX_categories", data);
  });

  app.ports.saveSession.subscribe(async (data: Session) => {
    await storage.save("EQUINOX_sessions", data);
  });
}
