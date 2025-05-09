import { sqliteTable, text, integer } from "drizzle-orm/sqlite-core";
import { sql } from "drizzle-orm";

export const breathingMethods = sqliteTable("breathing_methods", {
  // UUID形式の識別子
  id: text("id").primaryKey().unique(),

  // 1〜20文字の呼吸法名
  name: text("name").notNull(),

  // Posix時間（ミリ秒）
  createdAt: integer("created_at", { mode: "timestamp_ms" })
    .notNull()
    .default(sql`(CAST(unixepoch() * 1000 AS INTEGER))`),

  // 吸気時間（秒）、1秒以上10分以下
  inhale: integer("inhale").notNull(),

  // 吸気保持時間（秒）、0秒以上10分以下
  inhaleHold: integer("inhale_hold").notNull(),

  // 呼気時間（秒）、1秒以上10分以下
  exhale: integer("exhale").notNull(),

  // 呼気保持時間（秒）、0秒以上10分以下
  exhaleHold: integer("exhale_hold").notNull(),
});

// 型定義
export type BreathingMethod = typeof breathingMethods.$inferSelect;
export type NewBreathingMethod = typeof breathingMethods.$inferInsert;

export const users = sqliteTable("users", {
  // UUID形式の識別子
  id: text("id").primaryKey().unique(),
});

// 型定義
export type User = typeof users.$inferSelect;
