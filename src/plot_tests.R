ggplot(lst_models, aes(x = testing_response, y = testing_predicted, color = model_response)) +
  geom_point()

ggplot(lst_models[lst_models$model_response == "Acari",], aes(x = testing_response, y = testing_predicted, color = model_response)) +
  geom_point()

plot(testing_predicted ~ testing_response, lst_models[lst_models$model_response == "total_insct",])
abline(lm(testing_predicted ~ testing_response, lst_models[lst_models$model_response == "total_insct",]))
summary(lm(testing_predicted ~ testing_response, lst_models[lst_models$model_response == "total_insct",]))
