# ssearch

## 次やること

### 長い行を読み込む際にエラーがでる。

どうにかする。

### コマンド一覧

#### index

具体的にはdbにファイルと単語の転置インデックスを入れる。

1. pathにあるファイルコンテンツを形態素分解する。

2. 3以下の長さの形態素を除く。

3. DBに保存する。

	1. docs(id,path,contents\_hash)

	2. doc_words(word,doc\_id,linum)

#### search

検索する。
