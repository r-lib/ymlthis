# setup_chunk() captures chunk_code expressions

    Code
      cat(default)
    Output
      ```{r setup, include = FALSE}
      knitr::opts_chunk$set(echo = TRUE)
      ```

---

    Code
      cat(with_code)
    Output
      ```{r setup, include = FALSE}
      1 + 1
      ```

---

    Code
      cat(with_braces)
    Output
      ```{r setup, include = FALSE}
      library(dplyr)
      library(ggplot2)
      ```

# argument types are respected in code_chunk()

    Code
      cat(chunk1)
    Output
      ```{r fig.cap = "something"}
      plot(1:5)
      ```

---

    Code
      cat(chunk2)
    Output
      ```{r fig.cap = label_object}
      plot(1:5)
      ```

---

    Code
      cat(chunk3)
    Output
      ```{r fig.height = 3}
      plot(1:5)
      ```

