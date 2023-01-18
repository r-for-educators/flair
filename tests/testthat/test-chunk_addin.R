test_that("add_flair_chunk works", {
expect_equal(
  add_flair_chunk(
'
```{r name}
mean(x)
```
'
  ),
'
```{r name, include = FALSE}
mean(x)
```

```{r name_flair, echo = FALSE}
decorate("name")
```
'
)

expect_equal(
  add_flair_chunk(
'

```{r name}
mean(x)
```

'
  ),
'

```{r name, include = FALSE}
mean(x)
```

```{r name_flair, echo = FALSE}
decorate("name")
```

'
)

})

test_that("add_flair_chunk only affects first chunk", {
expect_equal(
  add_flair_chunk(
    '

```{r name}
mean(x)
```

```{r sum}
sum(x)
```

'
  ),
  '

```{r name, include = FALSE}
mean(x)
```

```{r name_flair, echo = FALSE}
decorate("name")
```

```{r sum}
sum(x)
```

'
)

})

test_that("add_flair_chunk flips include = TRUE", {
  expect_equal(
    add_flair_chunk(
      '

```{r name, include = TRUE}
mean(x)
```

```{r sum}
sum(x)
```

'
    ),
    '

```{r name, include = FALSE}
mean(x)
```

```{r name_flair, echo = FALSE}
decorate("name")
```

```{r sum}
sum(x)
```

'
  )

})

test_that("add_flair_chunk throws errors", {
  expect_error(
    add_flair_chunk(
      '
```{r name}
mean(x)
'
    )
  )

  expect_error(
    add_flair_chunk(
      '
```{r}
sum(x)
```
'
    )
  )

  expect_error(
    add_flair_chunk(
      '
```{r , , , , ,}
sum(x)
```
'
    )
  )

  expect_error(
    add_flair_chunk(
      '
```{r, eval=FALSE}
sum(x)
```
'
    )
  )

})
