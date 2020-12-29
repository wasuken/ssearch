# ssearch

## 次やること

### 転置インデックスによる検索実装

* 正規表現はサポートしない。

* よくあるクエリ(空白区切りだけサポート)を入力する。

#### コマンド一覧

##### index

具体的にはdbにファイルと単語の転置インデックスを入れる。

1. pathにあるファイルコンテンツを形態素分解する。

2. 3以下の長さの形態素を除く。

3. DBに保存する。

	1. words(id,name)

	2. docs(id,path,contents\_hash)

	3. doc_words(word\_id,doc\_id,linum)

##### search

検索する。
