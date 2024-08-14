--------------------------------------------------------------------------------
-- public.train のレイアウト変更
--   注意！！：テーブルに依存するオブジェクト（ビューなど）が削除される場合があります。それらのオブジェクトは復元されません。
--   2024/08/14 tateishi
--------------------------------------------------------------------------------


-- 新テーブルの作成
create table public."$$train" (
  "TD0" date
  , "TD1" bigint
  , "TD2" boolean
  , "TD3" boolean
  , "TD4" boolean
  , "TD5" boolean
  , "TD6" bigint
  , "TD7" bigint
  , "TD8" bigint
  , "TD9" bigint
  , "TD10" boolean
  , "TD11" boolean
  , "EV0" boolean
  , "EV1" real
  , "EV2" real
  , "EV3" smallint
  , "EV4" boolean
  , "EV5" smallint
  , "EV6" smallint
  , "EV7" smallint
  , "EV8" smallint
)
/


-- 新テーブルへデータ投入
insert into public."$$train"("TD0", "TD1", "TD2", "TD3", "TD4", "TD5", "TD6", "TD7", "TD8", "TD9", "TD10", "TD11", "EV0", "EV1", "EV2", "EV3", "EV4", "EV5", "EV6", "EV7", "EV8")
  select org."TD0", org."TD1", org."TD2", org."TD3", org."TD4", org."TD5", org."TD6", org."TD7", org."TD8", org."TD9", org."TD10", org."TD11", org."EV0", org."EV1", org."EV2", org."EV3", org."EV4", org."EV5", org."EV6", org."EV7", org."EV8" from public.train org
/


-- 元テーブルの削除
drop table public.train cascade
/


-- 新テーブルをリネームして元テーブル名に変更
alter table public."$$train" rename to train
/


-- コメントの作成
comment on table public.train is 'ml_test'
/

comment on column public.train."TD0" is '記録日'
/

comment on column public.train."TD1" is 'ビール摂取量(ml)'
/

comment on column public.train."TD2" is 'フルーツ摂取の有無'
/

comment on column public.train."TD3" is 'ヘパリーゼ摂取の有無'
/

comment on column public.train."TD4" is '首枕ストレッチの有無'
/

comment on column public.train."TD5" is '背中枕ストレッチの有無'
/

comment on column public.train."TD6" is '腹筋回数'
/

comment on column public.train."TD7" is '背筋回数'
/

comment on column public.train."TD8" is '腕立回数'
/

comment on column public.train."TD9" is 'スクワット回数'
/

comment on column public.train."TD10" is '有無'
/

comment on column public.train."TD11" is 'サイクリングの有無'
/

comment on column public.train."EV0" is '---'
/

comment on column public.train."EV1" is '握力(左)'
/

comment on column public.train."EV2" is '握力(右)'
/

comment on column public.train."EV3" is '寝起きの良さ(%)'
/

comment on column public.train."EV4" is '眼瞼ミオキミアの有無'
/

comment on column public.train."EV5" is '日中の眠気(%)'
/

comment on column public.train."EV6" is '体力(%)'
/

comment on column public.train."EV7" is '気力(%)'
/

comment on column public.train."EV8" is '寝付きの良さ(%)'
/


-- その他のDDL
grant DELETE on public.train to "postgres"
/

grant INSERT on public.train to "postgres"
/

grant REFERENCES on public.train to "postgres"
/

grant SELECT on public.train to "postgres"
/

grant TRIGGER on public.train to "postgres"
/

grant TRUNCATE on public.train to "postgres"
/

grant UPDATE on public.train to "postgres"
/

