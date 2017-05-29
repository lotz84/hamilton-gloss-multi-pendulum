n重振り子のシミュレーション
===========================

![](demo.png)

```
.
├── app
│   └── Main.hs -- ここに定義されてる数字をいじるとn重振り子のnを変えれます
└── src
    └── Pendulum
        ├── Model.hs -- n重振り子のデータ構造の定義と hamilton 用の関数の定義
        └── View.hs  -- n重振り子を描画する関数
```

使い方
------

```shell
$ git clone git@github.com:lotz84/hamilton-gloss-multi-pendulum.git
$ cd hamilton-gloss-multi-pendulum

$ stack build
$ stack exec hamilton-gloss-multi-pendulum-exe
```
