25: t.default(T)
24: t(T)
23: as.matrix(r)
22: backsolve(t(T), y, upper.tri = FALSE)
21: f(model, envir = envir.logLik)
20: (function (formula = ~1, design, response, covtype = "matern5_2", 
        coef.trend = NULL, coef.cov = NULL, coef.var = NULL, nugget = NULL, 
        nugget.estim = FALSE, noise.var = NULL, estim.method = "MLE", 
        penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
        parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
        iso = FALSE, scaling = FALSE, knots = NULL, kernel = NULL) 
    {
        if (!is.null(kernel)) {
            covtype <- "covUser"
            nugget.estim <- FALSE
        }
        model <- new("km")
        model@call <- match.call()
        data <- data.frame(design)
        model@trend.formula <- formula <- drop.response(formula, 
            data = data)
        F <- model.matrix(formula, data = data)
        X <- as.matrix(data)
        y <- as.matrix(response)
        model@X <- X
        model@y <- y
        model@d <- ncol(X)
        model@n <- nrow(X)
        model@F <- F
        model@p <- ncol(F)
        model@noise.flag <- (length(noise.var) != 0)
        model@noise.var <- as.numeric(noise.var)
        isTrend <- length(coef.trend) != 0
        isCov <- length(coef.cov) != 0
        isVar <- length(coef.var) != 0
        if ((isTrend && isCov && isVar) || (covtype == "covUser")) {
            known.param <- "All"
            nugget.estim <- FALSE
        }
        else if ((isTrend) && ((!isCov) || (!isVar))) {
            known.param <- "Trend"
        }
        else if ((!isTrend) && isCov && isVar) {
            known.param <- "CovAndVar"
            nugget.estim <- FALSE
        }
        else {
            known.param <- "None"
            coef.var <- coef.cov <- NULL
        }
        if (isCov) {
            known.covparam <- "All"
        }
        else {
            known.covparam <- "None"
        }
        model@covariance <- covStruct.create(covtype = covtype, d = model@d, 
            known.covparam = known.covparam, var.names = colnames(X), 
            coef.cov = coef.cov, coef.var = coef.var, nugget = nugget, 
            nugget.estim = nugget.estim, nugget.flag = ((length(nugget) != 
                0) || nugget.estim), iso = iso, scaling = scaling, 
            knots = knots, kernel = kernel)
        model@known.param <- known.param
        if (known.param == "All") {
            model@trend.coef <- as.numeric(coef.trend)
            model@param.estim <- FALSE
            validObject(model)
            model <- computeAuxVariables(model)
            return(model)
        }
        if (known.param == "CovAndVar") {
            model@param.estim <- TRUE
            validObject(model)
            model <- computeAuxVariables(model)
            x <- backsolve(t(model@T), model@y, upper.tri = FALSE)
            beta <- compute.beta.hat(x = x, M = model@M)
            z <- compute.z(x = x, M = model@M, beta = beta)
            model@z <- z
            model@trend.coef <- beta
            return(model)
        }
        if (known.param == "Trend") {
            model@trend.coef <- as.numeric(coef.trend)
        }
        if (length(penalty) == 0) {
            if (is.element(estim.method, c("MLE", "LOO"))) {
                model@method <- estim.method
            }
            else {
                stop("estim.method must be: 'MLE' or 'LOO'")
            }
        }
        else {
            if (covtype != "gauss") {
                stop("At this stage, Penalized Maximum Likelihood is coded only for Gaussian covariance")
            }
            penalty.set <- c("SCAD")
            if (!is.element(penalty$fun, penalty.set)) {
                stop("At this stage, the penalty #function has to be one of : SCAD")
            }
            if (length(penalty$value) == 0) {
                penalty$value <- sqrt(2 * log(model@n)/model@n) * 
                    seq(from = 1, by = 0.5, length = 15)
            }
            penalty$fun.derivative <- paste(penalty$fun, ".derivative", 
                sep = "")
            model@penalty <- penalty
            model@method <- "PMLE"
        }
        model@param.estim <- TRUE
        model@optim.method <- as.character(optim.method)
        if ((length(lower) == 0) || (length(upper) == 0)) {
            bounds <- covParametersBounds(model@covariance, design)
            if (length(lower) == 0) 
                lower <- bounds$lower
            if (length(upper) == 0) 
                upper <- bounds$upper
        }
        if ((multistart > 1) && (optim.method == "gen")) {
            warning("The 'multistart' argument is not used when 'optim.method' is 'gen'.")
            multistart <- 1
        }
        control$multistart <- multistart
        model@lower <- as.numeric(lower)
        model@upper <- as.numeric(upper)
        model@parinit <- as.numeric(parinit)
        if (optim.method == "BFGS") {
            if (length(control$pop.size) == 0) 
                control$pop.size <- 20
            control$pop.size <- max(control$pop.size, multistart)
            if (identical(control$trace, FALSE)) 
                control$trace <- 0
            if ((length(control$trace) == 0) || (identical(control$trace, 
                TRUE))) {
                control$trace <- 3
            }
        }
        if (optim.method == "gen") {
            d <- ncol(design)
            if (length(control$pop.size) == 0) 
                control$pop.size <- min(20, floor(4 + 3 * log(d)))
            if (length(control$max.generations) == 0) 
                control$max.generations <- 5
            if (length(control$wait.generations) == 0) 
                control$wait.generations <- 2
            if (length(control$BFGSburnin) == 0) 
                control$BFGSburnin <- 0
            if (identical(control$trace, FALSE)) {
                control$trace <- 0
            }
            else control$trace <- 1
        }
        upper.alpha <- control$upper.alpha
        if (length(upper.alpha) == 0) {
            control$upper.alpha <- 1 - 1e-08
        }
        else if ((upper.alpha < 0) || (upper.alpha > 1)) {
            control$upper.alpha <- 1 - 1e-08
        }
        model@control <- control
        model@gr <- as.logical(gr)
        envir.logLik <- new.env()
        validObject(model, complete = TRUE)
        varStationaryClass <- c("covTensorProduct", "covScaling", 
            "covAffineScaling", "covIso")
        if (length(noise.var) != 0) {
            model@case <- "LLconcentration_beta"
        }
        else if (!is.element(class(model@covariance), varStationaryClass)) {
            model@case <- "LLconcentration_beta"
        }
        else {
            knownNugget <- ((length(nugget) > 0) && (!nugget.estim))
            if (nugget.estim) {
                model@case <- "LLconcentration_beta_v_alpha"
            }
            else if (knownNugget) {
                model@case <- "LLconcentration_beta"
            }
            else {
                model@case <- "LLconcentration_beta_sigma2"
            }
        }
        if ((model@method == "LOO") & (model@case != "LLconcentration_beta_sigma2")) {
            stop("leave-One-Out is not available for this model")
        }
        f <- kmEstimate
        if (identical(model@method, "PMLE")) {
            cv <- function(lambda, object, f) {
                object@penalty$value <- lambda
                object@control$trace <- 0
                object <- f(object, envir = envir.logLik)
                criterion <- sum((object@y - leaveOneOut.km(object, 
                    type = "UK")$mean)^2)
                return(criterion)
            }
            lambda.val <- model@penalty$value
            nval <- length(lambda.val)
            u <- rep(0, nval)
            for (i in 1L:nval) {
                u[i] <- cv(lambda.val[i], object = model, f)
            }
            plot(lambda.val, u)
            lambda <- lambda.val[which.min(u)]
            model@penalty$value <- lambda
            model <- f(model, envir = envir.logLik)
        }
        else {
            model <- f(model, envir = envir.logLik)
        }
        return(model)
    })(design = list(max_depth = c(5, 6, 7, 11, 14, 9, 12, 3, 12, 
    13, 11, 8, 14, 9, 8, 6, 6, 3, 12, 10, 12, 14, 14, 9, 7, 5, 8, 
    15, 13, 7, 9, 9, 3, 4, 9, 9, 14, 4, 10, 4, 7, 8, 4, 10, 6, 4, 
    5, 12, 3, 14, 4, 9, 9, 3, 11, 8, 6, 9, 11, 10, 8, 15, 13, 5, 
    5, 9, 13, 3, 6, 9, 15, 10, 15, 14, 9, 11, 12, 8, 6, 4, 5), colsample_bytree = c(0.550960488407873, 
    0.327897030953318, 0.421146748587489, 0.895798925356939, 0.938120822887868, 
    0.695844944170676, 0.916175393550657, 0.773226435249671, 0.610977774555795, 
    0.404000250040554, 0.470680823852308, 0.794439160614274, 0.744374076463282, 
    0.94730015641544, 0.946983493561856, 0.905443682568148, 0.770838921447284, 
    0.991872781328857, 0.391399516514502, 0.563543377257884, 0.717809409089387, 
    0.636226188903674, 0.40803718615789, 0.883506833552383, 0.665099707245827, 
    0.975258489907719, 0.458895241073333, 0.800228521204553, 0.664578931964934, 
    0.805492117977701, 0.514310870808549, 0.670194060332142, 0.33333795464132, 
    0.683240383514203, 0.988003408396616, 0.393758743419312, 0.713212601700798, 
    0.858337764930911, 0.544967196462676, 0.634248472889885, 0.537320239469409, 
    0.618606849876232, 0.852034280332737, 0.854504967154935, 0.540658263047226, 
    0.710610581329092, 0.408699474972673, 0.720527966250665, 0.440680351201445, 
    0.377498627151363, 0.93718285469804, 0.41731521897018, 0.381553978379816, 
    0.735781327681616, 0.42067035282962, 0.414644804736599, 0.763645397918299, 
    0.743046198459342, 0.357291504344903, 0.958183780289255, 0.320763442385942, 
    0.482157845422626, 0.41776316810865, 0.69006309050601, 0.422334386757575, 
    0.316014571348205, 0.61839451356791, 0.736060559889302, 0.58605119951535, 
    0.412400399241596, 0.743132213875651, 0.898124053608626, 0.890804141224362, 
    0.708592670178041, 0.883661108533852, 0.555335249053314, 0.981473931763321, 
    0.982439498743042, 0.811302176886238, 0.794860456418246, 0.882734195701778
    ), subsample = c(0.515203526895493, 0.848249748209491, 0.685600105556659, 
    0.92144085499458, 0.471939593157731, 0.573198052216321, 0.439147665980272, 
    0.85326627488248, 0.700673570833169, 0.489036459452473, 0.682383731007576, 
    0.840857644169591, 0.436673590936698, 0.61574869225733, 0.748956995061599, 
    0.41631655942183, 0.807391411555, 0.947706184117123, 0.780753882997669, 
    0.883184877550229, 0.975117302173748, 0.423520530387759, 0.945870499592274, 
    0.311246093804948, 0.985855393554084, 0.371477443212643, 0.942557640676387, 
    0.407243085931987, 0.740121682942845, 0.781555349659175, 0.968379125185311, 
    0.310473727644421, 0.461385699664243, 0.731603993964381, 0.432568472344428, 
    0.54386283790227, 0.792753088171594, 0.856349922553636, 0.948895331495441, 
    0.865953055117279, 0.818387751281261, 0.510577249038033, 0.346804819512181, 
    0.937268804432824, 0.920562106929719, 0.831376821361482, 0.421350034046918, 
    0.458429384185001, 0.506523399357684, 0.688980426080525, 0.359412153949961, 
    0.899962348281406, 0.849827169720083, 0.568660870124586, 0.560034676571377, 
    0.534386242181063, 0.804581230296753, 0.452043531206436, 0.518883955432102, 
    0.681698514288291, 0.517054460453801, 0.68150128016714, 0.599400284211151, 
    0.676183159905486, 0.34422764533665, 0.670368726737797, 0.932226478285156, 
    0.993433376145549, 0.930258226441219, 0.778371535218321, 0.434015839057974, 
    0.376084766816348, 0.417882803827524, 0.875777398166247, 0.520405341801234, 
    0.35477232793346, 0.317206025565974, 0.959213483403437, 0.920932013378479, 
    0.712619380187243, 0.839298067009076), nrounds = c(1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), response = c(0.366103, 
    0.371388, 0.350583, 0.350369, 0.352543, 0.350551, 0.35066, 0.366746, 
    0.35054, 0.351252, 0.350689, 0.35045, 0.350802, 0.350466, 0.350457, 
    0.350675, 0.350536, 0.360468, 0.352848, 0.350393, 0.350275, 0.350831, 
    0.350567, 0.35262, 0.350331, 0.350946, 0.351455, 0.350584, 0.350418, 
    0.350362, 0.350347, 0.351036, 0.367847, 0.355053, 0.35081, 0.350466, 
    0.350369, 0.358355, 0.35125, 0.357705, 0.360176, 0.350524, 0.362235, 
    0.350264, 0.354181, 0.354278, 0.383408, 0.351734, 0.370914, 0.36555, 
    0.350935, 0.352416, 0.359195, 0.378641, 0.350574, 0.350688, 0.353263, 
    0.350753, 0.353433, 0.350425, 0.352223, 0.350745, 0.354314, 0.354834, 
    0.354269, 0.351641, 0.350385, 0.360624, 0.350683, 0.352097, 0.35066, 
    0.350711, 0.350783, 0.350458, 0.35077, 0.352539, 0.351097, 0.350401, 
    0.350253, 0.35973, 0.350328), covtype = "matern3_2", control = list(
        trace = FALSE))
19: do.call(DiceKriging::km, c(list(design = d$data, response = d$target), 
        args))
18: trainLearner.regr.km(.learner = list(id = "regr.km", type = "regr", 
        package = "DiceKriging", properties = c("numerics", "se"), 
        par.set = list(pars = list(covtype = list(id = "covtype", 
            type = "discrete", len = 1L, lower = NULL, upper = NULL, 
            values = list(gauss = "gauss", matern5_2 = "matern5_2", 
                matern3_2 = "matern3_2", exp = "exp", powexp = "powexp"), 
            cnames = NULL, allow.inf = FALSE, has.default = TRUE, 
            default = "matern5_2", trafo = NULL, requires = NULL, 
            tunable = TRUE, special.vals = list(), when = "train"), 
            coef.trend = list(id = "coef.trend", type = "numericvector", 
                len = NA_integer_, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), coef.cov = list(
                id = "coef.cov", type = "numericvector", len = NA_integer_, 
                lower = -Inf, upper = Inf, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), coef.var = list(id = "coef.var", 
                type = "numericvector", len = NA_integer_, lower = -Inf, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = FALSE, default = NULL, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), nugget = list(id = "nugget", type = "numeric", 
                len = 1L, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), nugget.estim = list(
                id = "nugget.estim", type = "logical", len = 1L, 
                lower = NULL, upper = NULL, values = list(`TRUE` = TRUE, 
                    `FALSE` = FALSE), cnames = NULL, allow.inf = FALSE, 
                has.default = TRUE, default = FALSE, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), noise.var = list(id = "noise.var", 
                type = "numericvector", len = NA_integer_, lower = -Inf, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = FALSE, default = NULL, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), estim.method = list(id = "estim.method", 
                type = "discrete", len = 1L, lower = NULL, upper = NULL, 
                values = list(MLE = "MLE", LOO = "LOO"), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = "MLE", 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), optim.method = list(id = "optim.method", 
                type = "discrete", len = 1L, lower = NULL, upper = NULL, 
                values = list(BFGS = "BFGS", gen = "gen"), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = "BFGS", 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), lower = list(id = "lower", type = "numericvector", 
                len = NA_integer_, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), upper = list(
                id = "upper", type = "numericvector", len = NA_integer_, 
                lower = -Inf, upper = Inf, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), parinit = list(id = "parinit", type = "numericvector", 
                len = NA_integer_, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), multistart = list(
                id = "multistart", type = "integer", len = 1L, lower = 1L, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = TRUE, default = 1L, trafo = NULL, requires = NULL, 
                tunable = TRUE, special.vals = list(), when = "train"), 
            control = list(id = "control", type = "untyped", len = 1L, 
                lower = NULL, upper = NULL, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), gr = list(id = "gr", type = "logical", 
                len = 1L, lower = NULL, upper = NULL, values = list(
                    `TRUE` = TRUE, `FALSE` = FALSE), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = TRUE, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), iso = list(id = "iso", type = "logical", 
                len = 1L, lower = NULL, upper = NULL, values = list(
                    `TRUE` = TRUE, `FALSE` = FALSE), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = FALSE, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), scaling = list(id = "scaling", type = "logical", 
                len = 1L, lower = NULL, upper = NULL, values = list(
                    `TRUE` = TRUE, `FALSE` = FALSE), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = FALSE, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), knots = list(id = "knots", type = "untyped", 
                len = 1L, lower = NULL, upper = NULL, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), jitter = list(
                id = "jitter", type = "logical", len = 1L, lower = NULL, 
                upper = NULL, values = list(`TRUE` = TRUE, `FALSE` = FALSE), 
                cnames = NULL, allow.inf = FALSE, has.default = TRUE, 
                default = FALSE, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "predict"), nugget.stability = list(
                id = "nugget.stability", type = "numeric", len = 1L, 
                lower = -Inf, upper = Inf, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = !nugget.estim && is.null(nugget), 
                tunable = TRUE, special.vals = list(), when = "train")), 
            forbidden = NULL), par.vals = list(jitter = FALSE, covtype = "matern3_2", 
            control = list(trace = FALSE)), predict.type = "se", 
        name = "Kriging", short.name = "km", note = "In predict, we currently always use `type = \"SK\"`. The extra parameter `jitter` (default is `FALSE`) enables adding a very small jitter (order 1e-12) to the x-values before prediction, as `predict.km` reproduces the exact y-values of the training data points, when you pass them in, even if the nugget effect is turned on. \n We further introduced `nugget.stability` which sets the `nugget` to `nugget.stability * var(y)` before each training to improve numerical stability. We recommend a setting of 10^-8", 
        config = list(on.learner.error = "stop"), fix.factors.prediction = TRUE), 
        .task = list(type = "regr", env = <environment>, weights = NULL, 
            blocking = NULL, task.desc = list(id = "data", type = "regr", 
                target = "y", size = 81L, n.feat = c(4L, 0L, 0L), 
                has.missings = FALSE, has.weights = FALSE, has.blocking = FALSE)), 
        .subset = 1:81, covtype = "matern3_2", control = list(trace = FALSE))
17: (function (.learner, .task, .subset, .weights = NULL, ...) 
    {
        UseMethod("trainLearner")
    })(.learner = list(id = "regr.km", type = "regr", package = "DiceKriging", 
        properties = c("numerics", "se"), par.set = list(pars = list(
            covtype = list(id = "covtype", type = "discrete", len = 1L, 
                lower = NULL, upper = NULL, values = list(gauss = "gauss", 
                    matern5_2 = "matern5_2", matern3_2 = "matern3_2", 
                    exp = "exp", powexp = "powexp"), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = "matern5_2", 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), coef.trend = list(id = "coef.trend", 
                type = "numericvector", len = NA_integer_, lower = -Inf, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = FALSE, default = NULL, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), coef.cov = list(id = "coef.cov", 
                type = "numericvector", len = NA_integer_, lower = -Inf, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = FALSE, default = NULL, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), coef.var = list(id = "coef.var", 
                type = "numericvector", len = NA_integer_, lower = -Inf, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = FALSE, default = NULL, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), nugget = list(id = "nugget", type = "numeric", 
                len = 1L, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), nugget.estim = list(
                id = "nugget.estim", type = "logical", len = 1L, 
                lower = NULL, upper = NULL, values = list(`TRUE` = TRUE, 
                    `FALSE` = FALSE), cnames = NULL, allow.inf = FALSE, 
                has.default = TRUE, default = FALSE, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), noise.var = list(id = "noise.var", 
                type = "numericvector", len = NA_integer_, lower = -Inf, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = FALSE, default = NULL, trafo = NULL, 
                requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), estim.method = list(id = "estim.method", 
                type = "discrete", len = 1L, lower = NULL, upper = NULL, 
                values = list(MLE = "MLE", LOO = "LOO"), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = "MLE", 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), optim.method = list(id = "optim.method", 
                type = "discrete", len = 1L, lower = NULL, upper = NULL, 
                values = list(BFGS = "BFGS", gen = "gen"), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = "BFGS", 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), lower = list(id = "lower", type = "numericvector", 
                len = NA_integer_, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), upper = list(
                id = "upper", type = "numericvector", len = NA_integer_, 
                lower = -Inf, upper = Inf, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), parinit = list(id = "parinit", type = "numericvector", 
                len = NA_integer_, lower = -Inf, upper = Inf, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), multistart = list(
                id = "multistart", type = "integer", len = 1L, lower = 1L, 
                upper = Inf, values = NULL, cnames = NULL, allow.inf = FALSE, 
                has.default = TRUE, default = 1L, trafo = NULL, requires = NULL, 
                tunable = TRUE, special.vals = list(), when = "train"), 
            control = list(id = "control", type = "untyped", len = 1L, 
                lower = NULL, upper = NULL, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), gr = list(id = "gr", type = "logical", 
                len = 1L, lower = NULL, upper = NULL, values = list(
                    `TRUE` = TRUE, `FALSE` = FALSE), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = TRUE, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), iso = list(id = "iso", type = "logical", 
                len = 1L, lower = NULL, upper = NULL, values = list(
                    `TRUE` = TRUE, `FALSE` = FALSE), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = FALSE, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), scaling = list(id = "scaling", type = "logical", 
                len = 1L, lower = NULL, upper = NULL, values = list(
                    `TRUE` = TRUE, `FALSE` = FALSE), cnames = NULL, 
                allow.inf = FALSE, has.default = TRUE, default = FALSE, 
                trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), 
                when = "train"), knots = list(id = "knots", type = "untyped", 
                len = 1L, lower = NULL, upper = NULL, values = NULL, 
                cnames = NULL, allow.inf = FALSE, has.default = FALSE, 
                default = NULL, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "train"), jitter = list(
                id = "jitter", type = "logical", len = 1L, lower = NULL, 
                upper = NULL, values = list(`TRUE` = TRUE, `FALSE` = FALSE), 
                cnames = NULL, allow.inf = FALSE, has.default = TRUE, 
                default = FALSE, trafo = NULL, requires = NULL, tunable = TRUE, 
                special.vals = list(), when = "predict"), nugget.stability = list(
                id = "nugget.stability", type = "numeric", len = 1L, 
                lower = -Inf, upper = Inf, values = NULL, cnames = NULL, 
                allow.inf = FALSE, has.default = FALSE, default = NULL, 
                trafo = NULL, requires = !nugget.estim && is.null(nugget), 
                tunable = TRUE, special.vals = list(), when = "train")), 
            forbidden = NULL), par.vals = list(jitter = FALSE, covtype = "matern3_2", 
            control = list(trace = FALSE)), predict.type = "se", 
        name = "Kriging", short.name = "km", note = "In predict, we currently always use `type = \"SK\"`. The extra parameter `jitter` (default is `FALSE`) enables adding a very small jitter (order 1e-12) to the x-values before prediction, as `predict.km` reproduces the exact y-values of the training data points, when you pass them in, even if the nugget effect is turned on. \n We further introduced `nugget.stability` which sets the `nugget` to `nugget.stability * var(y)` before each training to improve numerical stability. We recommend a setting of 10^-8", 
        config = list(on.learner.error = "stop"), fix.factors.prediction = TRUE), 
        .task = list(type = "regr", env = <environment>, weights = NULL, 
            blocking = NULL, task.desc = list(id = "data", type = "regr", 
                target = "y", size = 81L, n.feat = c(4L, 0L, 0L), 
                has.missings = FALSE, has.weights = FALSE, has.blocking = FALSE)), 
        .subset = 1:81, covtype = "matern3_2", control = list(trace = FALSE))
16: do.call(trainLearner, pars)
15: fun3(do.call(trainLearner, pars))
14: fun2(fun3(do.call(trainLearner, pars)))
13: fun1({
        learner.model = fun2(fun3(do.call(trainLearner, pars)))
    })
12: force(expr)
11: measureTime(fun1({
        learner.model = fun2(fun3(do.call(trainLearner, pars)))
    }))
10: train(learner, tasks[[i]])
9: force(expr)
8: measureTime({
       models[[i]] = train(learner, tasks[[i]])
   })
7: trainModels(learner = getOptProblemLearner(opt.problem), tasks = getOptStateTasks(opt.state), 
       control = getOptProblemControl(opt.problem))
6: getOptStateModels(opt.state)
5: (function (opt.state, par.set = NULL, control = NULL, opt.path = NULL, 
       models = NULL, ...) 
   {
       opt.problem = getOptStateOptProblem(opt.state)
       if (is.null(models)) 
           models = getOptStateModels(opt.state)$models
       models = if (inherits(models, "WrappedModel")) 
           list(models)
       else models
       par.set = coalesce(par.set, getOptProblemParSet(opt.problem))
       control = coalesce(control, getOptProblemControl(opt.problem))
       opt.path = coalesce(opt.path, getOptStateOptPath(opt.state))
       iter = getOptStateLoop(opt.state)
       infill.crit.id = getMBOInfillCritId(control$infill.crit)
       if (control$multifid) {
           infill.crit.id = "multifid"
       }
       n = control$propose.points
       prop.type = rep(paste0("infill_", infill.crit.id), n)
       ch = checkFailedModels(models, par.set, n, control = control)
       if (!ch$ok) 
           return(ch$prop)
       design = convertOptPathToDf(opt.path, control)
       infill.crit.fun = control$infill.crit$fun
       infill.opt.fun = getInfillOptFunction(control$infill.opt)
       secs = measureTime({
           prop.points = infill.opt.fun(infill.crit.fun, models = models, 
               control = control, par.set = par.set, opt.path = opt.path, 
               design = design, iter = iter, ...)
       })
       prop.points.converted = convertDataFrameCols(prop.points, 
           ints.as.num = TRUE, logicals.as.factor = TRUE)
       crit.vals = infill.crit.fun(prop.points.converted, models, 
           control, par.set, design, iter, attributes = TRUE, ...)
       crit.components = attr(crit.vals, "crit.components")
       crit.vals = matrix(crit.vals, ncol = 1L)
       makeProposal(control = control, prop.points = prop.points, 
           propose.time = secs, prop.type = prop.type, crit.vals = crit.vals, 
           crit.components = crit.components)
   })(control = dots[[1L]][[1L]], opt.state = <environment>)
4: mapply(fun2, ..., MoreArgs = more.args, SIMPLIFY = FALSE, USE.NAMES = FALSE)
3: parallelMap(proposePointsByInfillOptimization, control = controls, 
       level = "mlrMBO.propose.points", more.args = list(opt.state = opt.state))
2: proposePointsParallelCB(opt.state)
1: proposePoints(opt.state)
