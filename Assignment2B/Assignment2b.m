%% Exercise 1, a
clc, clear
% Find the maximum likelihood estimate mu_hat and beta_hat of mu and beta
% using the given data, and using such estimates show that the plot of
% corresponding pdf adapts well to the empirical distribution of the data.
% (for the latter you can use the 'histogram' function, using an
% appropriate 'Normalization' option for suitable comparison with the pdf.

% "The significant wave height is the average height of the highest
% one-third of all measured waves, which is equivalent to the estimate that
% would be made by a visual observer at sea."

% The data-file contain a collection of significant-wave-heights computed
% in different periods.
% The data file is named 'atlantic.txt'
wavedata = readtable("atlantic.txt", "LeadingDelimitersRule","ignore");
wavedata = table2array(wavedata);

% To find the maximum likelihood estimates of mu and beta from the data:
out = evfit(-wavedata); % Flipping the sign
mu_hat = -out(1); % Flipping the sign of mu est. to be compatible with eq 1
beta_hat = out(2);
% The maximum likelihood estimate of mu and beta are therefore mu_hat and
% beta_hat

% Limits chosen based on histogram of the dataset
x = 0:0.01:14;
% Generate the pdf for the plotting
f = 1/beta_hat .* exp(-1*(x-mu_hat)/beta_hat).*exp(-1*exp(-1*(x-mu_hat)/beta_hat));

figure
hold on
% Plot the pdf-line
plot(x, f, Color="k", LineWidth=2)
% Plot the histogram over the data, note the normalization method to be
% compatible with the pdf
histogram(wavedata, Normalization="pdf", FaceColor="b")
legend("pdf", "hist")
hold off
% Save the plot
saveas(gca, "Exercise1_a.png")

%% b
clc; clear; close all
% Before the for-loop in each question, place rng(123) to ease grading.
% Use parametric bootstrap (B = 2000) to estimate:
% 1. The distribution of estimated mu_hat, the distribution of the 
%    estimated mean of the significant wave-heigth. Plot the histogram.
% 2. Estimate the distribution of the distribution of the maximum value of 
%    the significant wave-height. Plot the histogram.
% Clarification: the distribution of the maximum value of the significant 
%    wave height = distribution of x_max = max(x1, .., xn)
% [ ] Estimate wave heigth in a loop see page 6.
% [ ] Estimate mean (mu_hat) of the generated data.
% [ ] Plot histogram. Title = "Distribution of mu_hat, using 1000 repeats"
% [ ] Take the largest x in each B = 1:1000, plot it. 

% Import the data as before
wavedata = readtable("atlantic.txt", "LeadingDelimitersRule","ignore");
wavedata = table2array(wavedata);

% Estimate the parameters as before
out = evfit(-wavedata); % Flipping the sign
% Flipping the sign of mu estimate to be compatible with eq 1:
mu_hat_original = -out(1);
beta_hat = out(2);

% Use a set sample size
sample_size = size(wavedata, 1); %The same size as the original dataset!
size_B = 2000; % The number of repeats.

% Preset these to the correct size, computationally more efficient
max_x = zeros(size_B, 1); 
estimated_mean = zeros(size_B, 1);
% Set the seed to ensure replicability
rng(123)
for B = 1:size_B
   % Estimate sample_size (n) new waveheights using the parameters from
    % the original data
    estimated_waveheight = -evrnd(-mu_hat_original, beta_hat, ...
                                  [sample_size 1]);
    % Fit a new model
    out_waveheight = evfit(-estimated_waveheight); % Flipping the sign
    % Obtain the estimated mu from the data
    % Flipping the sign of mu estimate to be compatible with eq 1:
    mu_hat(B) = -out_waveheight(1); 
    % Obtain the maximum waveheight in the sample and save it
    max_x(B) = max(estimated_waveheight);
end

% Plot the histograms and save them
figure
histogram(max_x, Normalization="pdf")
title("Max wave height")
saveas(gca, "Exercise1_b_max.png")

figure
histogram(mu_hat)
title("Estimated mu\_hat")
saveas(gca, "Exercise1_b_mu.png")


%% c
%clc; close all
% Based on question b), we wish to advise the mayor of a nearby city on
% building a wall which protects from even the highest wave, how high
% should this wall be?
max(max_x)
% 25.9068
% But the wall should be higher, as high as possible 
% Using B = 1 000 000, the maximum wave height is 32.7234 feet
% Since the probability of a larger wave than the current maximum wave
% height gets smaller the larger the current maximum wave height gets (the
% area for the density function above the current maximum wave height),
% there is a possibility, with an infinite amount of waves, of an even
% larger wave than the one mentioned here, and in fact the waves may get
% infinitely high, but the probability of it would also be very very small.
% I would advise the mayor to build the wall at least 35 feet high, since
% this would protect from the highest wave we saw during the million
% repeats. 


%% Exercise 2. i
clc; clear; close all
% Load in the correct data:
gamedata = readtable("gametime.txt");

% Create two new datasets, one for each group
game_control = gamedata(strcmp(gamedata.Group, 'Control'), :);
game_variant = gamedata(strcmp(gamedata.Group, 'Variant'), :);

% Use a set sample size, the same size as the original dataset!
sample_size_control = size(game_control, 1);
sample_size_variant = size(game_variant, 1);

% Set the number of repeats
size_B = 2000; 

% Preset these to the correct size, computationally more efficient
mean_control = zeros(size_B, 1); 
mean_variant = zeros(size_B, 1);

% Set the seed to ensure replicability
rng(123)
for B = 1:size_B
 % Sample new dataset from the original data, with replacement.
 sampled_control = randsample(game_control.IGT, sample_size_control, true);
 sampled_variant = randsample(game_variant.IGT, sample_size_variant, true);
 % Extract the mean values for each group
 mean_control(B) = mean(sampled_control);
 mean_variant(B) = mean(sampled_variant);
end

% Calculate the difference in mean IGT for the two groups for each B:
difference_treatments = mean_variant - mean_control;
% Calculate the probability that the mean IGT for the variant group is
% larger than the mean IGT for the control group.
prob_of_difference = sum(difference_treatments > 0) / size_B

% Plot the histograms for both mean values in the same figure
figure
hold on
histogram(mean_control)
histogram(mean_variant)
legend("Control", "Variant")
title("Mean IGT")
hold off
saveas(gca, "Exercise2_a_bothhist.png")


%% Exercise 2. ii
% Show the histogram of the distribution you previously obtained from the
% bootstrap

histogram(difference_treatments)
title("Difference in mean IGT (Variant - Control)")
saveas(gca, "Exercise2_b.png")


%% Exercise 2. iii (same as (i) except the 1 instead of 0 on the last line
clc; clear; close all

% Estimate the probability that the mean IGT for the variant group is at
% least one minute longer than the mean IGT for the control group.

% Load in the correct data:
gamedata = readtable("gametime.txt");

% Create two new datasets, one for each group
game_control = gamedata(strcmp(gamedata.Group, 'Control'), :);
game_variant = gamedata(strcmp(gamedata.Group, 'Variant'), :);

% Use a set sample size, the same size as the original dataset!
sample_size_control = size(game_control, 1);
sample_size_variant = size(game_variant, 1);

% Set the number of repeats
size_B = 2000; 

% Preset these to the correct size, computationally more efficient
mean_control = zeros(size_B, 1); 
mean_variant = zeros(size_B, 1);

% Set the seed to ensure replicability
rng(123)
for B = 1:size_B
 % Sample new dataset from the original data, with replacement.
 sampled_control = randsample(game_control.IGT, sample_size_control, true);
 sampled_variant = randsample(game_variant.IGT, sample_size_variant, true);
 % Extract the mean values for each group
 mean_control(B) = mean(sampled_control);
 mean_variant(B) = mean(sampled_variant);
end

% Calculate the difference in mean IGT for the two groups for each B:
difference_treatments = mean_variant - mean_control;
% Calculate the probability that the mean IGT for the variant group is
% at least 1 minute longer than the mean IGT for the control group.
prob_of_difference = sum(difference_treatments > 1) / size_B