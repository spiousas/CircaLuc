m1 <- cosinor.lm(Temperature ~ time(Time), period = 24, data = temperature_zg)
model_parameters(m1$fit)
summary(m1)
summary(m1$fit)

## using conditional linearity
D = 24
m2 <- nls(Temperature ~ alpha + amp * cos(2*pi*Time/24- acro),
                 data = temperature_zg,
                 start = list(alpha = 0, amp = 6, acro = 1))
summary(m2)
