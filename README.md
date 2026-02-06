* 项目结构

- blog.asd 永远在项目根目录。
- 源码永远在 src/。
- 运行时可写数据（posts、db、cache）不要写入仓库目录，而是写进用户目录/系统数据目录。



* DSL/SEXP 规范
文章 :content 里允许出现的节点

- Headline：(h1 "...") (h2 "...") (h3 "...")
- Paragraph（支持 inline 混排）：(p "text" (math (:mode "inline") "a^2") "more" (img (:src "x.png" :alt "x")) )
- Code block：(src (:lang "python") "print(1)\n")
- List：(ul (li ...) ...) + 新增 (ol ...)
- Image：(img (:src "..." :alt "..."))
- Math：(math (:mode "inline") "...") / (math (:mode "block") "...")



* Org 子集 → DSL 映射（只实现 5 项）

- Headline：* / ** / ***
- 代码块：#+begin_src LANG … #+end_src
- 数学：
  - 行内：$...$
  - 块级：$$…$$ 或 \[…\]
- 列表：
  - 无序：- item 或 + item
  - 有序：1. item
  - 嵌套：靠缩进（任意更深缩进都算嵌套）
- 图片（Org link）：
  - [[file:path]]
  - [[file:path][alt]]
  - [[https://...][alt]] → (img (:src ... :alt ...))（inline 出现在段落中也可）
