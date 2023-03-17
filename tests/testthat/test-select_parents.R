test_that(
  "Does select_parents select the parents according to correct probability?", {

    # Generate fake parents and fitness levels
    parents <- c("111", "000", "101", "010")
    fitness <- c(1, 2, 3, 4)


    # Probability of selection using rank is given by rank/(sum of all ranks)

    # Based on the parents and their fitness levels above, using rank based
    # selection parent "111" would get selected with probability 4/10,
    # parent "000" would get selected with probability 3/10,
    # parent "101" would get selected with probability 2/10,
    # parent "010" would get selected with probability 1/10

    # True probability of selecting parent 1
    probs1 <- c(0.4, 0.3, 0.2, 0.1)

    # True probability of selecting parent 2
    probs2 <- c(1/3*(1-(4/10)), 1/3*(1-(3/10)), 1/3*(1-(2/10)), 1/3*(1-(1/10)))

    # Number of simulations
    n <- 1000

    # Select 1001 sets of four parents using the select_parents function
    selected_parents <- data.frame(select_parents(parents, fitness))

    for (i in 1:n){
      selected_parents <- rbind(selected_parents,
                                select_parents(parents,
                                                fitness))
    }


    # Check that simulated proportion of selected parents is similar
    # to the parents known probability of selection

    # The for loop below checks that the proportion of parent 1's in the
    # simulated data is roughly equal to its known probability of selection
    for (i in 1:length(parents)){
      # print(sum(selected_parents$parent1s == parents[i])/nrow(selected_parents))
      # print(probs1[i])
      expect_true(sum(
        selected_parents$parent1s == parents[i])/nrow(selected_parents) - probs1[i] < 0.1)

    }

    # The for loop below checks that the proportion of parent 2's in
    # the simulated data is roughly equal to its known probability of selection
    for (i in 1:length(parents)){
      # print(sum(selected_parents$parent2s == parents[i])/nrow(selected_parents))
      # print(probs2[i])
      expect_true(sum(
        selected_parents$parent2s == parents[i])/nrow(selected_parents) - probs2[i] < 0.1)

    }
  })
