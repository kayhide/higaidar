module I18n.Ja where

import AppPrelude


higaidar :: String
higaidar = "病害虫集めるくん"


login :: String
login = "ログイン"

logout :: String
logout = "ログアウト"

populate :: String
populate = "一括登録"

submit :: String
submit = "送信"

revert :: String
revert = "撤回"


photo :: String
photo = "写真"

photo_list :: String
photo_list = photo <> "リスト"

take_photo :: String
take_photo = "写真を撮る"

select_crop :: String
select_crop = crop <> "を選ぶ"

select_pest :: String
select_pest = pest <> "を選ぶ"

no_crop :: String
no_crop = crop <> "未選択"

no_pest :: String
no_pest = pest <> "未選択"

without_hyphen :: String
without_hyphen = "ハイフンなし"

created_at :: String
created_at = "作成日時"

updated_at :: String
updated_at = "更新日時"

user :: String
user = "ユーザー"

user_list :: String
user_list = user <> "リスト"

user_name :: String
user_name = "名前"

user_code :: String
user_code = "お客様コード"

user_tel :: String
user_tel = "電話番号"

user_admin :: String
user_admin = "管理者"


crop :: String
crop = "作物"

crop_list :: String
crop_list = crop <> "リスト"

crop_label :: String
crop_label = crop <> "名"

pest :: String
pest = "病害虫"

pest_list :: String
pest_list = pest <> "リスト"

pest_label :: String
pest_label = pest <> "名"
