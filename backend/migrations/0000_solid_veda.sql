CREATE TABLE `breathing_methods` (
	`id` text PRIMARY KEY NOT NULL,
	`name` text NOT NULL,
	`created_at` integer DEFAULT (CAST(unixepoch() * 1000 AS INTEGER)) NOT NULL,
	`inhale` integer NOT NULL,
	`inhale_hold` integer NOT NULL,
	`exhale` integer NOT NULL,
	`exhale_hold` integer NOT NULL
);
