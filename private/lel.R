,
    visPerformances = function() {
      if (dim(self$bracket.storage$data.matrix)[1] == self$n.configs) {
        ggplot(data = self$bracket.storage$data.matrix, 
          aes(x = current_budget, y = y)) +
        scale_y_continuous(name = "performance") +
        scale_x_continuous(labels = function (x) floor(x), 
          name = "budget", limits = c(0, self$getBudgetAllocation() + 2)) +
        theme(legend.position = "none") +
        geom_point(size = 0.5, colour = "cyan3")
      } else {
        
      }
    }