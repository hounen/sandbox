--------------------------------------------------------------------------------
-- public.train のレイアウト変更
--   注意！！：テーブルに依存するオブジェクト（ビューなど）が削除される場合があります。それらのオブジェクトは復元されません。
--   2024/08/19 tateishi
--------------------------------------------------------------------------------


-- 新テーブルの作成
create table public."$$train" (
  "TD00000" date
  , "TD00100" bigint
  , "TD00200" boolean
  , "TD00300" boolean
  , "TD00310" boolean
  , "TD00320" smallint
  , "TD00321" smallint
  , "TD00322" smallint
  , "TD00400" boolean
  , "TD00500" boolean
  , "TD00600" bigint
  , "TD00700" bigint
  , "TD00800" bigint
  , "TD00900" bigint
  , "TD01000" boolean
  , "TD01100" boolean
  , "TD01200" time without time zone
  , "TD01300" time without time zone
  , "TD01400" time without time zone
  , "TD01500" time without time zone
  , "TD01600" time without time zone
  , "TD01700" boolean
  , "TD01710" boolean
  , "TD01720" boolean
  , "TD01730" boolean
  , "TD01740" boolean
  , "TD01750" boolean
  , "TD01760" boolean
  , "TD01770" boolean
  , "TD01780" boolean
  , "TD01790" boolean
  , "TD01800" boolean
  , "EV01200" boolean
  , "EV00100" real
  , "EV00200" real
  , "EV00300" smallint
  , "EV00400" boolean
  , "EV00410" time without time zone
  , "EV00500" smallint
  , "EV00600" smallint
  , "EV00700" smallint
  , "EV00800" smallint
)
/


-- 新テーブルへデータ投入
insert into public."$$train"("TD00000", "TD00100", "TD00200", "TD00300", "TD00310", "TD00320", "TD00321", "TD00322", "TD00400", "TD00500", "TD00600", "TD00700", "TD00800", "TD00900", "TD01000", "TD01100", "TD01200", "TD01300", "TD01400", "TD01500", "TD01600", "TD01700", "TD01710", "TD01720", "TD01730", "TD01740", "TD01750", "TD01760", "TD01770", "TD01780", "TD01790", "TD01800", "EV01200", "EV00100", "EV00200", "EV00300", "EV00400", "EV00410", "EV00500", "EV00600", "EV00700", "EV00800")
  select org."TD00000", org."TD00100", org."TD00200", org."TD00300", org."TD00310", org."TD00320", org."TD00321", org."TD00322", org."TD00400", org."TD00500", org."TD00600", org."TD00700", org."TD00800", org."TD00900", org."TD01000", org."TD01100", org."TD01200", org."TD01300", org."TD01400", org."TD01500", org."TD01600", org."TD01700", org."TD01710", org."TD01720", org."TD01730", org."TD01740", org."TD01750", org."TD01760", org."TD01770", org."TD01780", org."TD01790", org."TD01800", org."EV01200", org."EV00100", org."EV00200", org."EV00300", org."EV00400", org."EV00410", org."EV00500", org."EV00600", org."EV00700", org."EV00800" from public.train org
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

comment on column public.train."TD00000" is '記録日'
/

comment on column public.train."TD00100" is 'ビール摂取量(ml)'
/

comment on column public.train."TD00200" is 'フルーツ摂取の有無'
/

comment on column public.train."TD00300" is 'ヘパリーゼ摂取の有無'
/

comment on column public.train."TD00310" is '養命酒摂取の有無'
/

comment on column public.train."TD00320" is 'VB100(朝)(錠)'
/

comment on column public.train."TD00321" is 'VB100(昼)(錠)'
/

comment on column public.train."TD00322" is 'VB100(夜)(錠)'
/

comment on column public.train."TD00400" is '首枕ストレッチの有無'
/

comment on column public.train."TD00500" is '背中枕ストレッチの有無'
/

comment on column public.train."TD00600" is '腹筋回数'
/

comment on column public.train."TD00700" is '背筋回数'
/

comment on column public.train."TD00800" is '腕立回数'
/

comment on column public.train."TD00900" is 'スクワット回数'
/

comment on column public.train."TD01000" is '秘密の有無'
/

comment on column public.train."TD01100" is 'サイクリングの有無'
/

comment on column public.train."TD01200" is '起床時刻'
/

comment on column public.train."TD01300" is '朝食時刻'
/

comment on column public.train."TD01400" is '昼食時刻'
/

comment on column public.train."TD01500" is '夕食時刻'
/

comment on column public.train."TD01600" is '就寝時刻'
/

comment on column public.train."TD01700" is 'エソメプラゾールカプセル20mg(夕食後)'
/

comment on column public.train."TD01710" is 'ツムラ麻杏甘石湯エキス顆粒(医療用)(朝食前)'
/

comment on column public.train."TD01720" is 'ツムラ麻杏甘石湯エキス顆粒(医療用)(昼食前)'
/

comment on column public.train."TD01730" is 'ツムラ麻杏甘石湯エキス顆粒(医療用)(夕食前)'
/

comment on column public.train."TD01740" is 'デキストロメトルファン臭化水素酸塩錠15mg(朝食後)'
/

comment on column public.train."TD01750" is 'デキストロメトルファン臭化水素酸塩錠15mg(昼食後)'
/

comment on column public.train."TD01760" is 'デキストロメトルファン臭化水素酸塩錠15mg(夕食後後)'
/

comment on column public.train."TD01770" is 'カルボシステイン錠500mg(朝食後)'
/

comment on column public.train."TD01780" is 'カルボシステイン錠500mg(昼食後)'
/

comment on column public.train."TD01790" is 'カルボシステイン錠500mg(夕食後)'
/

comment on column public.train."TD01800" is 'カロナール錠500 500mg'
/

comment on column public.train."EV01200" is '---'
/

comment on column public.train."EV00100" is '握力(左)'
/

comment on column public.train."EV00200" is '握力(右)'
/

comment on column public.train."EV00300" is '寝起きの良さ(%)'
/

comment on column public.train."EV00400" is '眼瞼ミオキミアの有無'
/

comment on column public.train."EV00410" is '眼瞼ミオキミアの頻発時刻'
/

comment on column public.train."EV00500" is '日中の眠気(%)'
/

comment on column public.train."EV00600" is '体力(%)'
/

comment on column public.train."EV00700" is '気力(%)'
/

comment on column public.train."EV00800" is '寝付きの良さ(%)'
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

