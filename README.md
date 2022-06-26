Working with the Pokémon API
================
Michael Lightfoot \| 06/26/22





-   [Required Packages](#required-packages)
-   [Contacting the API](#contacting-the-api)
    -   [`pokemonMoves`](#pokemonmoves)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
-   [Conclusion](#conclusion)

This vignette explores the [Pokémon API](https://pokeapi.co/docs/v2),
and takes a deep dive into the moves that any Pokémon can learn.

## Required Packages

Below are the required packages to run this code:

-   `tidyverse`
-   `jsonlite`
-   `ggplot2`
-   `knitr`

## Contacting the API

We will define a few functions to help us contact the Pokémon API. In
terms of endpoints, this API has many groups of them. Below are the
groups with their respective endpoints:

-   **Berries** - Berries, Berry Firmnesses, and Berry Flavors.
-   **Contests** - Contest Types, Contest Effects, and Super Contest
    Effects
-   **Encounters** - Encounter Methods, Encounter Conditions, and
    Encounter Condition Values.
-   **Evolution** - Evolution Chains and Evolution Triggers
-   **Games** - Generations, Pokedexes, Version, and Version Groups.
-   **Items** - Item, Item Attributes, Item Categories, Item Fling
    Effects, and Item Pockets
-   **Locations** - Locations, Location Areas, Pal Park Areas, and
    Regions.
-   **Machines** - Machines
-   **Moves** - Moves, Move Ailments, Move Battle Styles, Move
    Categories, Move Damage Classes, Move Learn Methods, and Move
    Targets
-   **Pokémon** - Abilities, Characteristics, Egg Groups, Genders,
    Growth Rates, Natures, Pokeathlon Stats, Pokémon, Pokemon Location
    Areas, Pokemon Colors, Pokemon Forms, Pokemon Habitats, Pokemon
    Shapes, Stats, and Types
-   **Utility** - Languages and Common Models

In my eyes, the **Pokémon** and **Moves** groups seem to have some
interesting endpoints we should explore.

### `pokemonMoves`

Here we will define a function to grab all of the moves of any singular
Pokémon. The function input can either be the **id** (`integer` or
`string`) or **name** of the Pokémon (`string`), and the output will be
a `tibble` of that Pokémon’s possible moves with their respective names,
type, power, accuracy, power points, and damage class. This function
queries both the Pokémon and Moves endpoints within the **Pokémon** and
**Moves** groups respectively, and combines data from both. We have an
additional argument **stats** for customization of our output. The
function outputs all of the variables listed previously by default, but
**stats** can also be passed a list of any combination of the following
arguments to only output those variables:

-   “Name”: includes **Name** variable.
-   “Type”: includes **Type** variable.
-   “Power”: includes **Power** variable
-   “Accuracy”: includes **Accuracy** variable.
-   “PP”: includes **Power Points** variable.
-   “Class” : includes **Damage Class** variable.

``` r
pokemonMoves <- function(pokemon, stats="all") {
  ###
  # This function returns a tibble of a given Pokémon's moves 
  # and their names, type, power, accuracy and power points. 
  ###
  
  #Even if ID or Name is given, the process for getting the data is the same
  data  <- fromJSON(paste0('https://pokeapi.co/api/v2/pokemon/',tolower(pokemon)))
  moves <- data$moves$move
  n     <- length(moves[,1])
  
  #Initialize output data.frame with a dummy row we will overwrite
  output <- data.frame(Name = "chr", 
                       Type = "chr",
                       Power = 0,
                       Accuracy = 0,
                       PP = 0,
                       Class = "chr")
  
  #Loop through each move and grab desired variables for each move
  for (i in 1:n) {
    move    <- fromJSON(moves$url[i])
    #Initialize new row with dumm
    new     <- data.frame(Name = "chr", 
                          Type = "chr",
                          Power = 0,
                          Accuracy = 0,
                          PP = 0,
                          Class = "chr")
    new[,1]  <- move$name
    new[,2]  <- move$type$name
    #Moves without a power will be given a power of 0 for the sake of this 
    #analysis.
    if (length(move$power) == 0) {
      new[,3] <- 0
    } else {
      new[,3] <- move$power
    }
    #Moves with infinite accuracy will be given an accuracy score of 100 
    #for the sake of this analysis.
    if (length(move$accuracy) == 0) {
      new[,4]   <- 100
    } else {
      new[,4]   <- move$accuracy
    }
    new[,5]     <- move$pp
    new[,6]     <- move$damage_class[1]
    output[i,] <- new
  }
  
  #Now let's take into account the desired options
  if ("all" %in% stats) {
    output <- as_tibble(output)
  } else {
    if (all(stats %in% names(output))) {
      output <- as_tibble(output[stats])
    } else {
      stop("ERROR: Incorrect list of arguments for stats")
    }
  }
  
  return(output)
}
```

## Exploratory Data Analysis

Now, let us use these functions to explore some of the data this API can
give us. Let us call our first function, `pokemonMoves`, in order to
explore the moves of the iconic Pokémon Squirtle.

``` r
squirtleData <- pokemonMoves("squirtle")
```

Let’s look at the distribution of the different types of moves that
Squirtle gets.

``` r
kable(table(squirtleData$Type),"simple",
      col.names = c("Type", "Count"))
```

| Type     | Count |
|:---------|------:|
| dark     |     2 |
| dragon   |     2 |
| fighting |     9 |
| ghost    |     1 |
| ground   |     3 |
| ice      |     7 |
| normal   |    42 |
| poison   |     1 |
| psychic  |     5 |
| rock     |     3 |
| steel    |     3 |
| water    |    21 |

Let’s use a plot to help us visualize this a bit better, including a
color manual that utilizes the accepted colors for each type.

``` r
manual <- c("normal"   = "#A8A77A",
            "fire"     = "#EE8130",
            "water"    = "#6390F0",
            "electric" = "#F7D02C",
            "grass"    = "#7AC74C",
            "ice"      = "#96D9D6",
            "fighting" = "#C22E28",
            "poison"   = "#A33EA1",
            "ground"   = "#E2BF65",
            "flying"   = "#A98FF3",
            "psychic"  = "#F95587",
            "bug"      = "#A6B91A",
            "rock"     = "#B6A136",
            "ghost"    = "#735797",
            "dragon"   = "#6F35FC",
            "dark"     = "#705746",
            "steel"    = "#B7B7CE",
            "fairy"    = "#D685AD")

squirtleData$Type <- as.factor(squirtleData$Type)
plot1 <- ggplot(squirtleData, aes(x=Type, fill=Type)) + 
  geom_bar() + 
  labs(title = "Types of Squirtle's Moves", y = "Count") + 
  scale_fill_manual(values = manual)
plot1
```

![](../README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

It looks like most of Squirtle’s moves are either Normal or Water Type.
That makes sense, considering Squirtle is a Water Type Pokémon!

It would be nice to take a look at the different powers of moves within
each type. However, there are many different possibilities for the power
of a move. Let’s define some categories for the different strengths of
moves:

-   *Status*: Power is 0 - these moves have some alternate effect.
-   *Weak*: 0 &lt; Power ≤ 50
-   *Decent*: 50 &lt; Power ≤ 80
-   *Strong*: Power &gt; 80

Let’s add a variable called Strength that categorizes the Power variable
into these groups.

``` r
squirtleData$Strength <- 
  ifelse(squirtleData$Power == 0,"Status",
         ifelse(squirtleData$Power <= 50, "Weak",
                ifelse(squirtleData$Power <= 80, "Decent","Strong")))
squirtleData$Strength <- as.factor(squirtleData$Strength)
squirtleData$Strength <- ordered(squirtleData$Strength,
                                 levels = c("Status",
                                            "Weak",
                                            "Decent", 
                                            "Strong"))
```

With this new variable, let’s look at the distributions of different
types of Squirtle’s moves within these categories.

``` r
kable(table(squirtleData$Strength,squirtleData$Type), "simple")
```

|        | dark | dragon | fighting | ghost | ground | ice | normal | poison | psychic | rock | steel | water |
|--------|-----:|-------:|---------:|------:|-------:|----:|-------:|-------:|--------:|-----:|------:|------:|
| Status |    1 |      0 |        2 |     1 |      1 |   3 |     23 |      1 |       3 |    0 |     2 |     4 |
| Weak   |    0 |      0 |        2 |     0 |      1 |   0 |      7 |      0 |       1 |    1 |     0 |     4 |
| Decent |    1 |      0 |        3 |     0 |      1 |   2 |      7 |      0 |       1 |    2 |     0 |     8 |
| Strong |    0 |      2 |        2 |     0 |      0 |   2 |      5 |      0 |       0 |    0 |     1 |     5 |

Again, let’s take a visual look at distributions, with the color
representing Strength this time.

``` r
plot2 <- ggplot(squirtleData, aes(x=Type, fill=Strength)) + 
  geom_bar() + 
  labs(title = "Types of Squirtle's Moves", y = "Count") 
plot2
```

![](../README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

It looks like a major reason Squirtle has so many Normal Type moves is
because so many Status moves seem to be Normal Type.

Since we discuss the Power variable heavily from this point on, let’s
remove moves from the Status category for the remainder of our analysis,
as these moves all have a Power of zero.

``` r
squirtleData <- squirtleData %>%
  filter(squirtleData$Strength != "Status")
```

With this done, let’s look at a few numerical summaries of the Power
variable within the remaining Strength categories.

``` r
table1 <- squirtleData %>%
  group_by(Strength) %>%
  summarise(Mean = round(mean(Power)), 
            Median = round(median(Power)),
            SD = round(sd(Power)))
kable(table1, "simple")
```

| Strength | Mean | Median |  SD |
|:---------|-----:|-------:|----:|
| Weak     |   39 |     40 |   9 |
| Decent   |   71 |     75 |   9 |
| Strong   |  108 |    100 |  21 |

Now that we have an idea about the power of Squirtle’s moves, let’s
incorporate the accuracy of Squirtle’s moves into this discussion.
First, let’s plot the Power of Squirtle’s moves versus their Accuracy.

``` r
plot3 <- ggplot(squirtleData, aes(x=Accuracy, y=Power, color=Type, label=Name)) + 
  geom_point(position="jitter") + 
  labs(title = "Power vs Accuracy of Squirtle's Moves") + 
  scale_color_manual(values = manual)
plot3
```

![](../README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

However, this is not the whole story. In Pokémon, there is a mechanic
called **Same Type Attack Bonus**, or **STAB** for short. This increases
the effective power of moves that match a Pokémon’s type by 50%. So,
Squirtle’s Water Type moves technically should deal 50% more damage.
Let’s define a new variable to represent the **Effective Power** of
Squirtle’s moves.

``` r
squirtleData$effPower <- 
  ifelse(squirtleData$Type == "water",
         squirtleData$Power*1.5,
         squirtleData$Power)
```

If you would like to see which type(s) a Pokémon is to perform a similar
analysis on a different Pokémon, you can find your Pokémon’s types by
running the code below!

``` r
data <- fromJSON('https://pokeapi.co/api/v2/pokemon/{pokemon id or name}')
data$types$type$name
```

With this new variable defined, let us re-examine this plot of Power
versus Accuracy of Squirtle’s moves by using Effective Power instead of
Power.

``` r
plot4 <- ggplot(squirtleData, aes(x=Accuracy, y=effPower, color=Type, label=Name)) + 
  geom_point(position="jitter") +
  labs(title = "Effective Power vs Accuracy of Squirtle's Moves", y = "Effective Power") +
  scale_color_manual(values = manual)
plot4
```

![](../README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Now we can see some definite outlying Water Type moves by taking into
account Squirtle’s **STAB**.

We can create another categorical variable called **Risk** that splits
up Accuracy into the following groups:

-   *No Risk*: Accuracy is 100
-   *Low Risk*: 80 ≤ Accuracy &lt; 100
-   *High Risk*: Accuracy &lt; 80

``` r
squirtleData$Risk <- 
  ifelse(squirtleData$Accuracy == 100,"No Risk",
         ifelse(squirtleData$Accuracy >= 80, "Low Risk","High Risk"))
squirtleData$Risk <- as.factor(squirtleData$Risk)
squirtleData$Risk <- ordered(squirtleData$Risk,
                                 levels = c("No Risk",
                                            "Low Risk",
                                            "High Risk"))
```

Let’s look at the distribution of the Power of moves within these Risk
categories.

``` r
plot5 <- ggplot(squirtleData, aes(x=Risk, y=Power, fill = Risk)) + 
  geom_boxplot() + 
  labs(title = "Distributions of Power of Squirtle's Moves within Risk Categories")
plot5
```

![](../README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

These distributions of the Power within the different Risk categories
makes a fair bit of sense. The risky moves are generally strong, while
the weak and decent moves are generally less risky.

Let’s quantify this a little bit and see if we can find Squirtle’s
*best* move while taking into account both Power and Accuracy. If
Squirtle were to use a move with an Accuracy of 90%, then 10% of the
time the move will miss and therefore have an Effective Power of zero in
that case. So, the **True Power** of a move would be the overall average
of it’s possible Effective Power values. Let’s define a variable to
represent this.

$$\\text{True Power} = \\text{Effective Power} \\times \\frac{\\text{Accuracy}}{100} $$

``` r
squirtleData$truePower <- squirtleData$effPower*squirtleData$Accuracy/100
```

Let’s look at some numerical summaries of the True Power of Squirtle’s
for each Type.

``` r
table2<- squirtleData %>%
  group_by(Type) %>%
  summarise(Mean = round(mean(truePower)), 
            Median = round(median(truePower)),
            SD = round(sd(truePower)),
            Count = n())
kable(table2,"simple")
```

| Type     | Mean | Median |  SD | Count |
|:---------|-----:|-------:|----:|------:|
| dark     |   60 |     60 |  NA |     1 |
| dragon   |  102 |    102 |  25 |     2 |
| fighting |   71 |     64 |  38 |     7 |
| ground   |   50 |     50 |  42 |     2 |
| ice      |   74 |     76 |  16 |     4 |
| normal   |   67 |     68 |  27 |    19 |
| psychic  |   61 |     61 |  16 |     2 |
| rock     |   50 |     57 |  21 |     3 |
| steel    |   75 |     75 |  NA |     1 |
| water    |  106 |    115 |  41 |    17 |

Since True Power takes into account not only accuracy but also **STAB**,
it is not surprising that the Water Type has the highest mean True
Power. It is interesting that the Dragon Type comes pretty close with
only two observations, implying both Dragon Type moves Squirtle can
learn must be pretty strong.

Let’s look at the distributions of True Power within the different
Strength categories.

``` r
plot6 <- ggplot(squirtleData, aes(x = truePower, fill = Strength)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth = 10) + 
  geom_density(alpha = 0.5, position = "stack") +
  labs(title = "Distributions of True Power of Squirtle's Moves within Strength Categories",
       x = "True Power",
       y = "Density")
plot6
```

![](../README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

This plot is very interesting. Since True Power takes into account the
Accuracy of the moves, the Strong moves are not too different in
distribution from the Decent moves, aside from a few outliers. The fact
that many Strong moves are also risky clearly hinders that category a
bit when looking at True Power.

Lastly, let’s try to find Squirtle’s top ten moves, ranked by their True
Power.

``` r
squirtleData <- squirtleData %>%
  arrange(desc(truePower)) %>%
  select(Name, Type, Power, Accuracy, truePower)

kable(head(squirtleData, n=10), "simple",
      col.names = c("Name", "Type", "Power", "Accuracy", "True Power"))
```

| Name        | Type     | Power | Accuracy | True Power |
|:------------|:---------|------:|---------:|-----------:|
| water-spout | water    |   150 |      100 |      225.0 |
| focus-punch | fighting |   150 |      100 |      150.0 |
| surf        | water    |    90 |      100 |      135.0 |
| hydro-pump  | water    |   110 |       80 |      132.0 |
| skull-bash  | normal   |   130 |      100 |      130.0 |
| aqua-tail   | water    |    90 |       90 |      121.5 |
| double-edge | normal   |   120 |      100 |      120.0 |
| waterfall   | water    |    80 |      100 |      120.0 |
| outrage     | dragon   |   120 |      100 |      120.0 |
| dive        | water    |    80 |      100 |      120.0 |

From this table it seems **Water Spout** is Squirtle’s *strongest* move
with a True Power of 225!

## Conclusion

Of course, there are many factors not being taken into account in this
analysis. For example, Water Spout becomes a weaker move if Squirtle is
at lower HP. Additionally, we did not take into account the defending
Pokémon’s typing, which could affect the damage output. We did not take
into account the Class of the moves, and how that might affect damage
output. We did not take into account any additional effects of
Squirtle’s moves, which would add a layer of subjectivity to this
analysis. There are many ways to make our analysis more thorough, but I
think this was a fun start!

Thank you for coming along on this journey with me! I hope you enjoyed
taking a small dive into the Pokémon API.
