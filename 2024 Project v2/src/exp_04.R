
# Load the functions defined in functions.R
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/All_in_on.R")
source("C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/2024 Project v2/src/linear_tester.R")
library(viridis)
library(clue) 
library(gtools)


compare_clustering_methods <- function(normalize = "max", minNc = 3, maxNc = 10, 
                                       n_components = 2, year = 2015) {
  if(year != 0) {year_for_comparison <- year}
  
  # Perform clustering on the original features
  original_clusters <- clustering(normalize = normalize, minNc = minNc, maxNc = maxNc)
  
  # Perform clustering on the PCA-reduced features
  pca_clusters <- clustering_with_pca(normalize = normalize, minNc = minNc, 
                                      maxNc = maxNc, n_components = n_components)
  
  # Combine results into a single data frame for comparison
  comparison_df <- original_clusters %>%
    select(GEO, partition) %>%
    rename(Original_Cluster = partition) %>%
    left_join(pca_clusters %>% select(GEO, partition) %>% rename(PCA_Cluster = partition), 
              by = "GEO") %>%
    mutate(Match = ifelse(Original_Cluster == PCA_Cluster, "Same", "Different"))
  
  # Calculate the total number of points for annotation
  total_points <- nrow(comparison_df)
  same_count <- sum(comparison_df$Match == "Same")
  different_count <- total_points - same_count
  
  # Plot the differences in clustering between the two methods
  plot <- ggplot(comparison_df, aes(x = as.factor(Original_Cluster), 
                                    y = as.factor(PCA_Cluster))) +
    geom_jitter(aes(color = Match, shape = Match), 
                width = 0.2, height = 0.2, alpha = 0.7, size = 3) +
    scale_color_viridis_d(option = "plasma", end = 0.8, 
                          labels = c("Same Cluster", "Different Cluster"),
                          name = "Cluster Match") +
    scale_shape_manual(values = c(16, 17), 
                       labels = c("Same Cluster", "Different Cluster"),
                       name = "Cluster Match") +
    labs(title = "Comparison of Clustering Assignments: Original vs PCA-Reduced Features",
         subtitle = paste("Year for Comparison:", year_for_comparison),
         x = "Original Feature Clusters",
         y = "PCA-Reduced Feature Clusters",
         caption = paste("Total GEO Points:", total_points, 
                         "| Same:", same_count, 
                         "| Different:", different_count)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4)),
           shape = guide_legend(override.aes = list(size = 4)))
  
  # Optionally, add annotations for clarity
  plot <- plot +
    geom_text(data = comparison_df %>% filter(Match == "Different"),
              aes(label = GEO),
              vjust = -1, size = 3, color = "black", check_overlap = TRUE)
  
  # Print the plot
  print(plot)
  
  # Return the comparison data frame
  return(comparison_df)
}


compare_clustering_years_brute_force_old <- function(normalize = "Mean",
                                                 minNc = 3,
                                                 maxNc = 7,
                                                 year1 = 2005,
                                                 year2 = 2010) {
  # Validate input years
  if(year1 == 0 | year2 == 0){
    stop("Years for comparison must be non-zero values.")
  }
  
  # Perform clustering for the first year
  clusters_year1 <- clustering(normalize = normalize,
                               minNc = minNc,
                               maxNc = maxNc,
                               year = year1)
  
  # Perform clustering for the second year
  clusters_year2 <- clustering(normalize = normalize,
                               minNc = minNc,
                               maxNc = maxNc,
                               year = year2)
  
  # Ensure that both clusterings have the same number of clusters
  num_clusters_year1 <- length(unique(clusters_year1$partition))
  num_clusters_year2 <- length(unique(clusters_year2$partition))
  
  if(num_clusters_year1 != num_clusters_year2){
    warning(paste("Number of clusters in", year1, "(", num_clusters_year1, 
                  ") does not match number of clusters in", year2, 
                  "(", num_clusters_year2, "). Proceeding without alignment."))
    aligned <- FALSE
  } else {
    aligned <- TRUE
  }
  
  if(aligned){
    k <- num_clusters_year1
    cat("Number of clusters:", k, "\n")
    
    # Generate all possible permutations of cluster labels for year2
    permutations <- gtools::permutations(n = k, r = k, v = 1:k)
    
    # Initialize variables to track the best permutation
    max_matches <- -1
    best_permutation <- NULL
    
    # Convert cluster assignments to factors for consistency
    clusters_year1$partition <- as.factor(clusters_year1$partition)
    clusters_year2$partition <- as.factor(clusters_year2$partition)
    
    # Merge the two clusterings on GEO
    merged_clusters <- clusters_year1 %>%
      select(GEO, partition) %>%
      rename(Cluster_Year1 = partition) %>%
      left_join(clusters_year2 %>%
                  select(GEO, partition) %>%
                  rename(Cluster_Year2 = partition),
                by = "GEO")
    
    # Iterate through each permutation to find the best mapping
    for(i in 1:nrow(permutations)){
      perm <- permutations[i, ]
      # Create a mapping vector
      mapping <- setNames(perm, as.character(1:k))
      
      # Apply the permutation to Cluster_Year2
      aligned_clusters <- merged_clusters$Cluster_Year2
      aligned_clusters <- as.numeric(as.character(aligned_clusters))
      aligned_clusters <- mapping[as.character(aligned_clusters)]
      
      # Calculate the number of matches
      matches <- sum(merged_clusters$Cluster_Year1 == aligned_clusters)
      
      # Update the best permutation if current one has more matches
      if(matches > max_matches){
        max_matches <- matches
        best_permutation <- mapping
      }
    }
    
    cat("Best permutation:", paste(names(best_permutation), "->", best_permutation, collapse = ", "), "\n")
    cat("Maximum matches:", max_matches, "/", nrow(merged_clusters), "\n")
    
    # Apply the best permutation to Cluster_Year2
    merged_clusters$Cluster_Year2_aligned <- as.numeric(as.character(merged_clusters$Cluster_Year2))
    merged_clusters$Cluster_Year2_aligned <- best_permutation[as.character(merged_clusters$Cluster_Year2_aligned)]
    
  } else {
    # If not aligned, proceed without alignment
    merged_clusters <- clusters_year1 %>%
      select(GEO, partition) %>%
      rename(Cluster_Year1 = partition) %>%
      left_join(clusters_year2 %>%
                  select(GEO, partition) %>%
                  rename(Cluster_Year2_aligned = partition),
                by = "GEO")
  }
  
  # Create the comparison column
  merged_clusters <- merged_clusters %>%
    mutate(Match = ifelse(Cluster_Year1 == Cluster_Year2_aligned, "Same", "Different"))
  
  # Ensure 'Match' is treated as a factor with correct levels for plotting
  merged_clusters$Match <- factor(merged_clusters$Match, levels = c("Same", "Different"))
  
  # Create the plot
  plot <- ggplot(merged_clusters, aes(x = as.factor(Cluster_Year1),
                                      y = as.factor(Cluster_Year2_aligned))) +
    geom_jitter(aes(color = Match, shape = Match),
                width = 0.2, height = 0.2, alpha = 0.7, size = 3) +
    scale_color_viridis_d(option = "plasma",
                          end = 0.8,
                          labels = c("Same Cluster", "Different Cluster"),
                          name = "Cluster Match") +
    scale_shape_manual(values = c(16, 17),  # Define shapes for "Same" and "Different"
                       labels = c("Same Cluster", "Different Cluster"),
                       name = "Cluster Match") +
    labs(x = paste(year1, "Clusters"),
         y = paste(year2, "Clusters"),
         caption = paste("Total GEO Points:", nrow(merged_clusters), 
                         "| Same:", sum(merged_clusters$Match == "Same"), 
                         "| Different:", sum(merged_clusters$Match == "Different"))) +
    theme_minimal(base_size = 14) +
    theme(
      plot.subtitle = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4)),
           shape = guide_legend(override.aes = list(size = 4)))
  
  # Add annotations for GEO points with different cluster assignments
  plot <- plot +
    geom_text(data = merged_clusters %>% filter(Match == "Different"),
              aes(label = GEO),
              vjust = -1, size = 3, color = "black", check_overlap = TRUE)
  
  # Print the plot
  print(plot)
  
  # Return the comparison data frame
  return(merged_clusters)
}

# Define the compare function
compare_clustering_years_brute_force <- function(
    normalize = "Mean",
    minNc = 3,
    maxNc = 7,
    reference_year = 2018,
    first_year = 2020,
    last_year = 2005,
    force_same_no_of_clusters_as_the_reference = TRUE
) {
  # Load necessary libraries
  library(dplyr)
  library(ggplot2)
  library(gtools)  # For permutations
  
  # Validate year range
  if (first_year < last_year) {
    stop("first_year should be greater than or equal to last_year.")
  }
  
  # Perform clustering for the reference year
  cat("Clustering for reference year:", reference_year, "\n")
  
  if (force_same_no_of_clusters_as_the_reference) {
    # Use original clustering to determine the best number of clusters
    clusters_reference_temp <- clustering(normalize = normalize,
                                          minNc = minNc,
                                          maxNc = maxNc,
                                          year = reference_year)
    num_clusters_reference <- length(unique(clusters_reference_temp$partition))
    # Now perform clustering with k = num_clusters_reference
    clusters_reference <- clustering_with_k(normalize = normalize,
                                            k = num_clusters_reference,
                                            year = reference_year)
  } else {
    # Use original clustering function to determine the best number of clusters
    clusters_reference <- clustering(normalize = normalize,
                                     minNc = minNc,
                                     maxNc = maxNc,
                                     year = reference_year)
    num_clusters_reference <- length(unique(clusters_reference$partition))
  }
  
  # Ensure 'partition' is treated as a factor
  clusters_reference$partition <- as.factor(clusters_reference$partition)
  
  # Initialize a data frame to store all comparisons
  all_comparisons <- data.frame()
  
  # Define the years to compare against the reference year
  comparison_years <- seq(first_year, last_year, by = -1)
  comparison_years <- setdiff(comparison_years, reference_year)
  
  # Loop over each comparison year
  for (year in comparison_years) {
    cat("Processing year:", year, "\n")
    
    # Perform clustering for the comparison year
    if (force_same_no_of_clusters_as_the_reference) {
      clusters_comparison <- clustering_with_k(normalize = normalize,
                                               k = num_clusters_reference,
                                               year = year)
    } else {
      clusters_comparison <- clustering(normalize = normalize,
                                        minNc = minNc,
                                        maxNc = maxNc,
                                        year = year)
    }
    
    # Ensure 'partition' is treated as a factor
    clusters_comparison$partition <- as.factor(clusters_comparison$partition)
    
    # Get number of clusters in comparison year
    num_clusters_comparison <- length(unique(clusters_comparison$partition))
    
    # Initialize aligned cluster column
    clusters_comparison$Cluster_aligned <- NA
    
    # Merge clusters on GEO
    merged_clusters <- clusters_reference %>%
      select(GEO, partition) %>%
      rename(Cluster_reference = partition) %>%
      left_join(clusters_comparison %>%
                  select(GEO, partition) %>%
                  rename(Cluster_comparison = partition),
                by = "GEO")
    
    # Initialize best match count and mapping
    best_match_count <- -1
    best_mapping <- list()
    
    # Function to generate all possible mappings
    generate_mappings <- function(ref_clusters, comp_clusters) {
      mappings <- list()
      ref_labels <- levels(ref_clusters)
      comp_labels <- levels(comp_clusters)
      k <- length(ref_labels)
      l <- length(comp_labels)
      
      if (l == k) {
        # Case 1: Same number of clusters
        perms <- gtools::permutations(n = l, r = l, v = comp_labels)
        for (i in 1:nrow(perms)) {
          mapping <- setNames(as.character(perms[i, ]), comp_labels)
          mappings[[length(mappings) + 1]] <- mapping
        }
      } else if (l < k) {
        # Case 2: Fewer comparison clusters
        ref_combinations <- combn(ref_labels, l, simplify = FALSE)
        for (ref_subset in ref_combinations) {
          perms <- gtools::permutations(n = l, r = l, v = comp_labels)
          for (i in 1:nrow(perms)) {
            mapping <- setNames(as.character(perms[i, ]), comp_labels)
            mappings[[length(mappings) + 1]] <- mapping
          }
        }
      } else {
        # Case 3: More comparison clusters
        comp_combinations <- combn(comp_labels, k, simplify = FALSE)
        for (comp_subset in comp_combinations) {
          perms <- gtools::permutations(n = k, r = k, v = comp_subset)
          for (i in 1:nrow(perms)) {
            mapping <- setNames(as.character(perms[i, ]), comp_subset)
            # Assign "Unmatched" to remaining comparison clusters
            unmatched_comps <- setdiff(comp_labels, comp_subset)
            for (comp in unmatched_comps) {
              mapping[[comp]] <- "Unmatched"
            }
            mappings[[length(mappings) + 1]] <- mapping
          }
        }
      }
      return(mappings)
    }
    
    # Generate all possible mappings
    mappings <- generate_mappings(
      ref_clusters = clusters_reference$partition,
      comp_clusters = clusters_comparison$partition
    )
    
    cat("Total mappings to evaluate for year", year, ":", length(mappings), "\n")
    
    # Iterate through each mapping to find the best one
    for (mapping in mappings) {
      # Apply the mapping to Cluster_comparison
      aligned_clusters <- merged_clusters$Cluster_comparison
      aligned_clusters <- as.character(aligned_clusters)
      
      # Replace cluster labels with mapped reference labels
      aligned_clusters_mapped <- sapply(aligned_clusters, function(x) {
        if (x %in% names(mapping)) {
          return(mapping[[x]])
        } else {
          return("Unmatched")
        }
      })
      
      # Calculate the number of matches
      matches <- sum(merged_clusters$Cluster_reference == aligned_clusters_mapped, na.rm = TRUE)
      
      # Update the best mapping if current one has more matches
      if (matches > best_match_count) {
        best_match_count <- matches
        best_mapping <- mapping
        best_aligned_clusters <- aligned_clusters_mapped
      }
    }
    
    cat("Best match count for year", year, ":", best_match_count, "/", nrow(merged_clusters), "\n")
    
    # Assign the best aligned clusters
    merged_clusters$Cluster_aligned <- best_aligned_clusters
    
    # Create the comparison column
    merged_clusters <- merged_clusters %>%
      mutate(Match = ifelse(Cluster_reference == Cluster_aligned, "Same", "Different"),
             Year = year)
    
    # Append to the all_comparisons data frame
    all_comparisons <- bind_rows(all_comparisons, merged_clusters)
  }
  
  # Convert columns to factors for plotting
  all_comparisons$Cluster_reference <- as.factor(all_comparisons$Cluster_reference)
  all_comparisons$Cluster_aligned <- as.factor(all_comparisons$Cluster_aligned)
  all_comparisons$Match <- factor(all_comparisons$Match, levels = c("Same", "Different"))
  
  # Calculate total counts of "Same" and "Different"
  total_same <- sum(all_comparisons$Match == "Same", na.rm = TRUE)
  total_different <- sum(all_comparisons$Match == "Different", na.rm = TRUE)
  
  # Create the plot
  plot <- ggplot(all_comparisons, aes(x = Cluster_reference,
                                      y = Cluster_aligned)) +
    geom_jitter(aes(color = Match),
                width = 0.2, height = 0.2, alpha = 0.6, size = 2) +
    scale_color_manual(values = c("Same" = "blue", "Different" = "red"),
                       labels = c("Same Cluster", "Different Cluster"),
                       name = "Cluster Match") +
    labs(x = paste(reference_year, "Clusters"),
         y = "Aligned Clusters (Other Years)",
         caption = paste("Total Same Clusters:", total_same, 
                         "| Total Different Clusters:", total_different)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black")
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
  
  # Print the plot
  print(plot)
  
  # Return the comparison data frame
  return(all_comparisons)
}





clustering(normalize = "Mean", minNc = 3, maxNc = 7, year = 2005)

visualize_clustering_results(normalize = "Mean", use_pca = FALSE, year = 2018)

compare_clustering_methods(normalize = "Mean", minNc = 3, maxNc = 5, year = 2006)

# Example call to the new function
result_df <- compare_clustering_years_brute_force_old(normalize = "Mean",
                                      minNc = 3,
                                      maxNc = 7,
                                      year1 = 2018,
                                      year2 = 2006)

compare_clustering_years_brute_force(normalize = "Mean",
                                     minNc = 3,
                                     maxNc = 7,
                                     reference_year = 2018,
                                     first_year = 2020,
                                     last_year = 2005,
                                     force_same_no_of_clusters_as_the_reference = FALSE)
#Save cluster Comparison 2018 vs. 2005 - 2020

#Generate barplot of the average free allocation of each country seperated as clusters. 

# Create a data frame mapping GEO to Cluster

this_is_not_a_function_just_a_huge_parenthesis <- function(){
  # Create a data frame mapping GEO to Cluster
  
  # Create a data frame mapping GEO to Cluster
  geo_cluster <- data.frame(
    GEO = unlist(clusters),
    Cluster = rep(1:length(clusters), times = sapply(clusters, length)),
    stringsAsFactors = FALSE
  )
  
  # Define phase based on year
  get_phase <- function(year) {
    if(year >= 2005 & year <= 2007){
      return("Phase 1")
    } else if(year >= 2008 & year <= 2012){
      return("Phase 2")
    } else if(year >= 2013 & year <= 2020){
      return("Phase 3")
    } else {
      return(NA)
    }
  }
  
  # Read data from 2005 to 2020 and combine using only read_data_2
  years <- 2005:2020
  
  # Function to read data
  read_and_prepare <- function(year) {
    df <- read_data_2(year = year)
    df$Year <- year
    return(df)
  }
  
  # Combine all data into one data frame
  all_data <- lapply(years, read_and_prepare) %>% bind_rows()
  
  # Assign Phase and remove NAs in Free and Population
  all_data <- all_data %>%
    mutate(Phase = sapply(Year, get_phase)) %>%
    filter(!is.na(Free) & !is.na(Population))  # Remove rows with NA Free or Population
  
  # Merge with cluster info (only one Cluster column)
  all_data <- all_data %>%
    inner_join(geo_cluster, by = "GEO")  # Only include GEOs in clusters
  
  # Calculate average Free and average Free per capita per GEO per Phase
  avg_free <- all_data %>%
    group_by(GEO, Phase) %>%
    summarize(
      Avg_Free = mean(Free, na.rm = TRUE),
      Avg_Free_per_capita = mean(Free / Population, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Ensure all combinations of GEO and Phase exist, filling missing with 0
  avg_free <- avg_free %>%
    complete(GEO, Phase, fill = list(Avg_Free = 0, Avg_Free_per_capita = 0)) 
  
  # Merge with geo_cluster to get Cluster information (avoid duplicate Cluster columns)
  avg_free <- avg_free %>%
    left_join(geo_cluster, by = "GEO") %>%
    arrange(Cluster, GEO)
  
  # Get ordered list of GEOs
  ordered_geos <- avg_free %>%
    distinct(GEO, Cluster) %>%
    arrange(Cluster, GEO) %>%
    pull(GEO)
  
  # Define the factor with ordered GEOs
  avg_free$GEO <- factor(avg_free$GEO, levels = ordered_geos)
  
  # Calculate cluster sizes
  cluster_sizes <- sapply(clusters, length)
  
  # Compute cumulative cluster sizes
  cumulative_sizes <- cumsum(cluster_sizes)
  
  # Positions for geom_vline (between clusters)
  vline_positions <- cumulative_sizes[-length(cumulative_sizes)] + 0.5
  
  # Define colors for phases
  phase_colors <- c("Phase 1" = "#1f78b4", 
                    "Phase 2" = "#ff7f00", 
                    "Phase 3" = "#33a02c")
  
  # Function to create and save a plot
  create_and_save_plot <- function(data, y_var, y_label, plot_title, plot_filename, log_scale = FALSE) {
    # If log_scale, set zeros to 1 to avoid -Inf
    if(log_scale){
      data <- data %>%
        mutate(!!sym(y_var) := ifelse(!!sym(y_var) == 0, 1, !!sym(y_var)))
    }
    
    p <- ggplot(data, aes(x = GEO, y = !!sym(y_var), fill = Phase)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      scale_fill_manual(values = phase_colors) +
      labs(
        x = "Country",
        y = y_label,
        fill = "Phase",
        title = plot_title,
        subtitle = "Phases: Phase 1 (2005-2007), Phase 2 (2008-2012), Phase 3 (2013-2020)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      # Add vertical lines to separate clusters
      geom_vline(xintercept = vline_positions, linetype = "dashed", color = "grey50") +
      # Adjust x-axis expansion to make space for vertical lines
      scale_x_discrete(expand = expansion(add = c(0.5, 0.5)))
    
    # Apply logarithmic scale if specified
    if(log_scale){
      p <- p + scale_y_continuous(trans = 'log10') +
        labs(y = paste("Log10", y_label))
    }
    
    # Save the plot
    ggsave(plot_filename, plot = p, width = 14, height = 8)
    
    return(p)
  }
  
  # Define file paths (ensure these directories exist)
  plot_dir <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/04/"
  plot1_path <- paste0(plot_dir, "average_free_allocation.png")
  plot2_path <- paste0(plot_dir, "average_free_allocation_log.png")
  plot3_path <- paste0(plot_dir, "average_free_allocation_per_capita.png")
  
  # Create and save Plot 1: Average Free Allocation
  plot1 <- create_and_save_plot(
    data = avg_free,
    y_var = "Avg_Free",
    y_label = "Average Free Allocation",
    #plot_title = "Average Free Allocation Across Countries and Phases",
    plot_title = "",
    plot_filename = plot1_path,
    log_scale = FALSE
  )
  
  # Create and save Plot 2: Average Free Allocation with Logarithmic Y-Scale
  plot2 <- create_and_save_plot(
    data = avg_free,
    y_var = "Avg_Free",
    y_label = "Average Free Allocation",
    plot_title = "",
    #plot_title = "Average Free Allocation Across Countries and Phases (Log Scale)",
    plot_filename = plot2_path,
    log_scale = TRUE
  )
  
  # Create and save Plot 3: Average Free Allocation per Capita
  plot3 <- create_and_save_plot(
    data = avg_free,
    y_var = "Avg_Free_per_capita",
    y_label = "Average Free Allocation per Capita",
    plot_title = "",
    #plot_title = "Average Free Allocation per Capita Across Countries and Phases",
    plot_filename = plot3_path,
    log_scale = FALSE
  )
  
  # Optionally, display the plots in the R session
  print(plot1)
  print(plot2)
  print(plot3)  # Optionally, display the plots in the R session


}

features_linear_free(select = 1, attribute = "Population")
plot_correlation_matrix(2015)


Peirama_1 <-function(){
  temp <- read_data_2(year = 2004)
  temp$Phase <- "0"
  temp$year <- 2004
  temp2 <- read_data_2(year = 2005)
  temp2$Phase <- "Phase I"
  temp2$year <- 2005
  temp <- rbind(temp, temp2)
  for (i in 2006:2007){
    temp2 <- read_data_2(year = i)
    temp2$Phase <- "Phase I"
    temp2$year <- i
    temp <- rbind(temp, temp2)
  }
  for (i in 2008:2012){
    temp2 <- read_data_2(year = i)
    temp2$Phase <- "Phase II"
    temp2$year <- i
    temp <- rbind(temp, temp2)
  }
  for (i in 2013:2020){
    temp2 <- read_data_2(year = i)
    temp2$Phase <- "Phase III"
    temp2$year <- i
    temp <- rbind(temp, temp2)
  }
  
  # Find which cluster they belong to from the list "clusters"
  for (i in 1:nrow(temp)){
    for (j in 1:6){
      if (temp$GEO[i] == clusters[[1]][j]){
        temp$partition[i] <- "First"
      }
    }
    for (j in 1:6){
      if (temp$GEO[i] == clusters[[2]][j]){
        temp$partition[i] <- "Second"
      }
    }    
    for (j in 1:13){
      if (temp$GEO[i] == clusters[[3]][j]){
        temp$partition[i] <- "Third"
      }
    }
  }
  
  
  temp$actual_agri <- temp$Agriculture*temp$GDPpc*temp$Population
  temp$actual_ind <- temp$Industry*temp$GDPpc*temp$Population
  temp$actual_manu <- temp$Manufacturing*temp$GDPpc*temp$Population
  temp$tot_and_EI <- temp$Total_energy_supply*temp$Energy_Intensity
  
  temp$last_verified <- 0
  for (i in 1:nrow(temp)){
    a <- temp[i,]
    for (j in 1:nrow(temp)){
      b <- temp[j,]
      if (a$GEO == b$GEO & a$year == b$year + 1){
        temp$last_verified[i] <- b$Verified_emissions
      }
    }
  }
  temp[which(temp$GEO == "Greece"),][c(1,2,3,13,19)]
  
  
  
  
  
  
  #Linear for Paper
  temp$Total_ener_times_EI <- temp$Total_energy_supply*temp$Energy_Intensity
  temp_phaseIII <- temp[which(temp$Phase=="Phase III"),]
  Lin <- lm(temp_phaseIII$Free ~ temp_phaseIII$Population + temp_phaseIII$GDPpc + temp_phaseIII$Total_ener_times_EI)
  summary(Lin)
  theme_set(theme_minimal())
  
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase I")] ~ temp$Verified_emissions[which(temp$Phase=="Phase I")])
  summary(ff)
  ggplot(temp[which(temp$Phase=="Phase I"),], aes(x = Verified_emissions, y = Free))+
    geom_point(aes(color = partition)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("Verified emissions in  t CO2 equivalent ") + 
    ylab("Free Allocation in t CO2 equivalent") +
    #scale_x_log10()+
    #scale_y_log10() +
    labs(title = "Phase I", color = "Cluster")
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase II")] ~ temp$Verified_emissions[which(temp$Phase=="Phase II")])
  summary(ff)
  ggplot(temp[which(temp$Phase=="Phase II"),], aes(x = Verified_emissions, y = Free))+
    geom_point(aes(color = partition)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("Verified emissions in  t CO2 equivalent ") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase II", color = "Cluster")
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase III")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III")])
  summary(ff)
  ggplot(temp[which(temp$Phase=="Phase III"),], aes(x = Verified_emissions, y = Free))+
    geom_point(aes(color = partition)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    xlab("Verified emissions in  t CO2 equivalent ") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "3,Phase III", color = "Cluster")
  
  
  ff <- lm(temp$Free[which(temp$Phase=="Phase III" & temp$partition == "First")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III"& temp$partition == "First")])
  summary(ff)
  f2 <- lm(temp$Free[which(temp$Phase=="Phase III" & temp$partition == "Second")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III"& temp$partition == "Second")])
  summary(f2)
  f3 <- lm(temp$Free[which(temp$Phase=="Phase III" & temp$partition == "Third")] ~ temp$Verified_emissions[which(temp$Phase=="Phase III"& temp$partition == "Third")])
  summary(f3)
  ggplot(temp[which(temp$Phase=="Phase III" & temp$partition=="First"),])+
    geom_point( aes(x = Verified_emissions, y = Free, alpha = year, color = GEO))+ 
    geom_smooth(aes(x = Verified_emissions, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Verified_emissions, y = Free, color = GEO, alpha = year))+
    #scale_x_log10()+
    #scale_y_log10() +
    xlab("Verified emissions in  t CO2 equivalent ") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase III First Cluster", color = "Country")
  
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = Population, y = Free, color = partition))+ 
    geom_smooth(aes(x = Population, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "5,year = 2019", color = "Cluster")
  
  
  
  
  ############################verified Phase III ###################################
  # Calculate the linear models for each cluster
  # Calculate the linear models for each cluster
  lm1 <- coef(lm(temp$Free[temp$Phase == "Phase III" & temp$partition == "First"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase III" & temp$partition == "First"]))
  lm2 <- coef(lm(temp$Free[temp$Phase == "Phase III" & temp$partition == "Second"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase III" & temp$partition == "Second"]))
  lm3 <- coef(lm(temp$Free[temp$Phase == "Phase III" & temp$partition == "Third"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase III" & temp$partition == "Third"]))
  
  # Print summaries if needed
  print(summary(lm(temp$Free[temp$Phase == "Phase III" & temp$partition == "First"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase III" & temp$partition == "First"])))
  print(summary(lm(temp$Free[temp$Phase == "Phase III" & temp$partition == "Second"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase III" & temp$partition == "Second"])))
  print(summary(lm(temp$Free[temp$Phase == "Phase III" & temp$partition == "Third"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase III" & temp$partition == "Third"])))
  
  # Create the plot
  image <- ggplot(temp[temp$Phase == "Phase III", ]) +
    geom_point(aes(x = Verified_emissions, y = Free, color = partition, alpha = year)) + 
    geom_abline(intercept = lm1[1], slope = lm1[2], color = rgb(248, 118, 100, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2[1], slope = lm2[2], color = rgb(0, 186, 56, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3[1], slope = lm3[2], color = rgb(97, 156, 255, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_line(aes(x = Verified_emissions, y = Free, group = GEO, alpha = year)) +
    xlab("Verified emissions in t CO2 equivalent") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase III", color = "Cluster") 
  
  # Print the plot
  print(image)
  #######################################################################################
  ############################verified Phase II ###################################
  # Calculate the linear models for each cluster
  # Calculate the linear models for each cluster in Phase II
  lm1 <- coef(lm(temp$Free[temp$Phase == "Phase II" & temp$partition == "First"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase II" & temp$partition == "First"]))
  lm2 <- coef(lm(temp$Free[temp$Phase == "Phase II" & temp$partition == "Second"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase II" & temp$partition == "Second"]))
  lm3 <- coef(lm(temp$Free[temp$Phase == "Phase II" & temp$partition == "Third"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase II" & temp$partition == "Third"]))
  
  # Print summaries if needed
  print(summary(lm(temp$Free[temp$Phase == "Phase II" & temp$partition == "First"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase II" & temp$partition == "First"])))
  print(summary(lm(temp$Free[temp$Phase == "Phase II" & temp$partition == "Second"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase II" & temp$partition == "Second"])))
  print(summary(lm(temp$Free[temp$Phase == "Phase II" & temp$partition == "Third"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase II" & temp$partition == "Third"])))
  
  # Create the plot
  image <- ggplot(temp[temp$Phase == "Phase II", ]) +
    geom_point(aes(x = Verified_emissions, y = Free, color = partition, alpha = year)) + 
    geom_abline(intercept = lm1[1], slope = lm1[2], color = rgb(248, 118, 100, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2[1], slope = lm2[2], color = rgb(0, 186, 56, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3[1], slope = lm3[2], color = rgb(97, 156, 255, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_line(aes(x = Verified_emissions, y = Free, group = GEO, alpha = year)) +
    xlab("Verified emissions in t CO2 equivalent") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase II", color = "Cluster") 
  
  # Print the plot
  print(image)
  
  #######################################################################################
  ############################verified Phase I ###################################
  # Calculate the linear models for each cluster
  # Calculate the linear models for each cluster in Phase I
  lm1 <- coef(lm(temp$Free[temp$Phase == "Phase I" & temp$partition == "First"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase I" & temp$partition == "First"]))
  lm2 <- coef(lm(temp$Free[temp$Phase == "Phase I" & temp$partition == "Second"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase I" & temp$partition == "Second"]))
  lm3 <- coef(lm(temp$Free[temp$Phase == "Phase I" & temp$partition == "Third"] ~ 
                   temp$Verified_emissions[temp$Phase == "Phase I" & temp$partition == "Third"]))
  
  # Print summaries if needed
  print(summary(lm(temp$Free[temp$Phase == "Phase I" & temp$partition == "First"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase I" & temp$partition == "First"])))
  print(summary(lm(temp$Free[temp$Phase == "Phase I" & temp$partition == "Second"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase I" & temp$partition == "Second"])))
  print(summary(lm(temp$Free[temp$Phase == "Phase I" & temp$partition == "Third"] ~ 
                     temp$Verified_emissions[temp$Phase == "Phase I" & temp$partition == "Third"])))
  
  # Create the plot
  image <- ggplot(temp[temp$Phase == "Phase I", ]) +
    geom_point(aes(x = Verified_emissions, y = Free, color = partition, alpha = year)) + 
    geom_abline(intercept = lm1[1], slope = lm1[2], color = rgb(248, 118, 100, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2[1], slope = lm2[2], color = rgb(0, 186, 56, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3[1], slope = lm3[2], color = rgb(97, 156, 255, maxColorValue = 255), 
                linetype = "dashed", size = 1) +
    geom_line(aes(x = Verified_emissions, y = Free, group = GEO, alpha = year)) +
    xlab("Verified emissions in t CO2 equivalent") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase I", color = "Cluster") 
  
  # Print the plot
  print(image)
  
  #######################################################################################
  
  
  ###################################Population Phase III  
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$Population[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$Population[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$Population[which(temp$Phase == "Phase III" & temp$partition=="Third")]))
  
  image6 <- ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    xlab("Population") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase III", color = "Cluster") 
  print(image6)
  ggsave(file="6.svg", plot=image6, path = "./Paper4pages/graphs", width = 6)
  
  ####################### Population Phase II
  
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="First")] ~temp$Population[which(temp$Phase == "Phase II" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Second")] ~temp$Population[which(temp$Phase == "Phase II" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Third")] ~temp$Population[which(temp$Phase == "Phase II" & temp$partition=="Third")]))
  
  summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="First")] ~temp$Population[which(temp$Phase == "Phase II" & temp$partition=="First")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Second")] ~temp$Population[which(temp$Phase == "Phase II" & temp$partition=="Second")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Third")] ~temp$Population[which(temp$Phase == "Phase II" & temp$partition=="Third")]))
  image61 <- ggplot(temp[which(temp$Phase == "Phase II"),])+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    xlab("Population") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase II", color = "Cluster") 
  print(image61)
  
  ####################### Population Phase I
  
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="First")] ~temp$Population[which(temp$Phase == "Phase I" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Second")] ~temp$Population[which(temp$Phase == "Phase I" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Third")] ~temp$Population[which(temp$Phase == "Phase I" & temp$partition=="Third")]))
  
  summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="First")] ~temp$Population[which(temp$Phase == "Phase I" & temp$partition=="First")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Second")] ~temp$Population[which(temp$Phase == "Phase I" & temp$partition=="Second")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Third")] ~temp$Population[which(temp$Phase == "Phase I" & temp$partition=="Third")]))
  image62 <- ggplot(temp[which(temp$Phase == "Phase I"),])+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), 
                linetype="dashed", size=1)+
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    xlab("Population") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase I", color = "Cluster") 
  print(image62)
  
  
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Population, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Population") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "7, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = Population, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Population, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Population, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Population") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",8 All", color = "Cluster")
  
  
  
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = GDPpc, y = Free, color = partition))+ 
    geom_smooth(aes(x = GDPpc, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "9,year = 2019", color = "Cluster")
  
  
  ##################################### GDP per capita Phase III
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~ temp$GDPpc[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~ temp$GDPpc[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~ temp$GDPpc[which(temp$Phase == "Phase III" & temp$partition=="Third")])) 
  
  image10 <- ggplot(temp[which(temp$Phase == "Phase III"),]) +
    geom_point(aes(x = GDPpc, y = Free, color = partition, alpha = year)) + 
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year)) +
    geom_abline(intercept = lm1$coefficients[1, 1], slope = lm1$coefficients[2, 1], 
                color = rgb(248, 118, 100, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2$coefficients[1, 1], slope = lm2$coefficients[2, 1], 
                color = rgb(0, 186, 56, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3$coefficients[1, 1], slope = lm3$coefficients[2, 1], 
                color = rgb(97, 156, 255, maxColorValue = 255), linetype = "dashed", size = 1) +
    xlab("GDPpc") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase III", color = "Cluster") 
  
  print(image10)
  
  ggsave(file="10.svg", plot=image10, path = "./Paper4pages/graphs", width = 6)
  ########################################################################
  ##################################### GDP per capita Phase II
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="First")] ~ temp$GDPpc[which(temp$Phase == "Phase II" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Second")] ~ temp$GDPpc[which(temp$Phase == "Phase II" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Third")] ~ temp$GDPpc[which(temp$Phase == "Phase II" & temp$partition=="Third")])) 
  
  image111 <- ggplot(temp[which(temp$Phase == "Phase II"),]) +
    geom_point(aes(x = GDPpc, y = Free, color = partition, alpha = year)) + 
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year)) +
    geom_abline(intercept = lm1$coefficients[1, 1], slope = lm1$coefficients[2, 1], 
                color = rgb(248, 118, 100, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2$coefficients[1, 1], slope = lm2$coefficients[2, 1], 
                color = rgb(0, 186, 56, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3$coefficients[1, 1], slope = lm3$coefficients[2, 1], 
                color = rgb(97, 156, 255, maxColorValue = 255), linetype = "dashed", size = 1) +
    xlab("GDPpc") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase II", color = "Cluster") 
  
  print(image111)
  
  ggsave(file="10.svg", plot=image10, path = "./Paper4pages/graphs", width = 6)
  ########################################################################
  ##################################### GDP per capita Phase II
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="First")] ~ temp$GDPpc[which(temp$Phase == "Phase I" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Second")] ~ temp$GDPpc[which(temp$Phase == "Phase I" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Third")] ~ temp$GDPpc[which(temp$Phase == "Phase I" & temp$partition=="Third")])) 
  
  image1111 <- ggplot(temp[which(temp$Phase == "Phase I"),]) +
    geom_point(aes(x = GDPpc, y = Free, color = partition, alpha = year)) + 
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year)) +
    geom_abline(intercept = lm1$coefficients[1, 1], slope = lm1$coefficients[2, 1], 
                color = rgb(248, 118, 100, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2$coefficients[1, 1], slope = lm2$coefficients[2, 1], 
                color = rgb(0, 186, 56, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3$coefficients[1, 1], slope = lm3$coefficients[2, 1], 
                color = rgb(97, 156, 255, maxColorValue = 255), linetype = "dashed", size = 1) +
    xlab("GDPpc") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase I", color = "Cluster") 
  
  print(image1111)
  
  ggsave(file="10.svg", plot=image10, path = "./Paper4pages/graphs", width = 6)
  ########################################################################
  
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = GDPpc, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = GDPpc, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of GDPpc") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "11, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = GDPpc, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = GDPpc, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = GDPpc, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of GDPpc") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",12 All", color = "Cluster")
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition))+ 
    geom_smooth(aes(x = Total_energy_supply, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "13,year = 2019", color = "Cluster")
  
  
  
  ######################### Total Energy Supply Phase III
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase III" & temp$partition=="Third")])) 
  
  image14 <- ggplot(temp[which(temp$Phase == "Phase III"),]) +
    geom_point(aes(x = Total_energy_supply, y = Free, color = partition, alpha = year)) + 
    geom_abline(intercept = lm1$coefficients[1, 1], slope = lm1$coefficients[2, 1], 
                color = rgb(248, 118, 100, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2$coefficients[1, 1], slope = lm2$coefficients[2, 1], 
                color = rgb(0, 186, 56, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3$coefficients[1, 1], slope = lm3$coefficients[2, 1], 
                color = rgb(97, 156, 255, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year)) +
    xlab("Total_energy_supply") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase III", color = "Cluster") 
  
  print(image14)
  ggsave(file="14.svg", plot=image14, path = "./Paper4pages/graphs", width = 6)
  ######################################################
  ######################### Total Energy Supply Phase II
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="First")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase II" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Second")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase II" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Third")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase II" & temp$partition=="Third")])) 
  
  image141 <- ggplot(temp[which(temp$Phase == "Phase II"),]) +
    geom_point(aes(x = Total_energy_supply, y = Free, color = partition, alpha = year)) + 
    geom_abline(intercept = lm1$coefficients[1, 1], slope = lm1$coefficients[2, 1], 
                color = rgb(248, 118, 100, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2$coefficients[1, 1], slope = lm2$coefficients[2, 1], 
                color = rgb(0, 186, 56, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3$coefficients[1, 1], slope = lm3$coefficients[2, 1], 
                color = rgb(97, 156, 255, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year)) +
    xlab("Total_energy_supply") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase II", color = "Cluster") 
  
  print(image141)
  ggsave(file="14.svg", plot=image14, path = "./Paper4pages/graphs", width = 6)
  ######################################################
  ######################### Total Energy Supply Phase II
  lm1 <- summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="First")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase I" & temp$partition=="First")]))
  lm2 <- summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Second")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase I" & temp$partition=="Second")]))
  lm3 <- summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Third")] ~ temp$Total_energy_supply[which(temp$Phase == "Phase I" & temp$partition=="Third")])) 
  
  image1411 <- ggplot(temp[which(temp$Phase == "Phase I"),]) +
    geom_point(aes(x = Total_energy_supply, y = Free, color = partition, alpha = year)) + 
    geom_abline(intercept = lm1$coefficients[1, 1], slope = lm1$coefficients[2, 1], 
                color = rgb(248, 118, 100, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm2$coefficients[1, 1], slope = lm2$coefficients[2, 1], 
                color = rgb(0, 186, 56, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_abline(intercept = lm3$coefficients[1, 1], slope = lm3$coefficients[2, 1], 
                color = rgb(97, 156, 255, maxColorValue = 255), linetype = "dashed", size = 1) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year)) +
    xlab("Total_energy_supply") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase I", color = "Cluster") 
  
  print(image1411)
  ggsave(file="14.svg", plot=image14, path = "./Paper4pages/graphs", width = 6)
  ######################################################
  
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "15, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = Total_energy_supply, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",16 All", color = "Cluster")
  
  ggplot(temp[which(temp$year==2019),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition))+ 
    geom_smooth(aes(x = Total_energy_supply*Energy_Intensity, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    labs(title = "17,year = 2019", color = "Cluster")
  
  
  ################################## tot time EI ####################################
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="Third")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="First")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="Second")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase III" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase III" & temp$partition=="Third")]))
  
  
  image18 <- ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), linetype="dashed", size=1)+
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    xlab("Total_energy_supply Energy_Intensity") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase III", color = "Cluster") 
  print(image18)
  ggsave(file="18.svg", plot=image18, path = "./Paper4pages/graphs", width = 6)
  ################################################################################
  ################################## tot time EI ####################################
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase II" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase II" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase II" & temp$partition=="Third")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase II" & temp$partition=="First")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase II" & temp$partition=="Second")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase II" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase II" & temp$partition=="Third")]))
  
  
  image181 <- ggplot(temp[which(temp$Phase == "Phase II"),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), linetype="dashed", size=1)+
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    xlab("Total_energy_supply Energy_Intensity") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase II", color = "Cluster") 
  print(image181)
  ggsave(file="18.svg", plot=image18, path = "./Paper4pages/graphs", width = 6)
  ################################################################################
  ################################## tot time EI ####################################
  lm1 <- coef(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase I" & temp$partition=="First")]))
  lm2 <- coef(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase I" & temp$partition=="Second")]))
  lm3 <- coef(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase I" & temp$partition=="Third")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="First")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase I" & temp$partition=="First")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Second")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase I" & temp$partition=="Second")]))
  summary(lm(temp$Free[which(temp$Phase == "Phase I" & temp$partition=="Third")] ~temp$Total_ener_times_EI[which(temp$Phase == "Phase I" & temp$partition=="Third")]))
  
  
  image1811 <- ggplot(temp[which(temp$Phase == "Phase I"),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_abline(intercept = lm1[1] , slope = lm1[2], color=rgb(248, 118, 100,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm2[1] , slope = lm2[2], color=rgb(0, 186, 56,maxColorValue=255), linetype="dashed", size=1)+
    geom_abline(intercept = lm3[1] , slope = lm3[2], color=rgb(97, 156, 255,maxColorValue=255), linetype="dashed", size=1)+
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    xlab("Total_energy_supply Energy_Intensity") + 
    ylab("Free Allocation in t CO2 equivalent") +
    labs(title = "Phase I", color = "Cluster") 
  print(image1811)
  ggsave(file="18.svg", plot=image18, path = "./Paper4pages/graphs", width = 6)
  ################################################################################
  
  
  
  ggplot(temp[which(temp$Phase == "Phase III"),])+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply*Energy_Intensity, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply*Energy_Intensity") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = "19, Phase III", color = "Cluster")
  
  ggplot(temp)+
    geom_point( aes(x = Total_energy_supply*Energy_Intensity, y = Free, color = partition, alpha = year))+ 
    geom_smooth(aes(x = Total_energy_supply*Energy_Intensity, y = Free),method = "lm", se = FALSE, color = "black", size =0.5) +
    geom_line(aes(x = Total_energy_supply*Energy_Intensity, y = Free, group = GEO, alpha = year))+
    scale_x_log10()+
    scale_y_log10() +
    xlab("Log 10 of Total_energy_supply*Energy_Intensity") + 
    ylab("Log 10 of Free Allocation in t CO2 equivalent") +
    labs(title = ",20 All", color = "Cluster")
  
  
  tep <- temp[-which(temp$Free == 0),]
  tep <- tep[-which(tep$last_verified == 0),]  
  tep$year <- as.integer(tep$year)
  lm1 <- lm(tep$Free[which(tep$Phase == "Phase I")] ~tep$last_verified[which(tep$Phase == "Phase I")])
  lm2 <- lm(tep$Free[which(tep$Phase == "Phase II")] ~tep$last_verified[which(tep$Phase == "Phase II")])
  lm3 <- lm(tep$Free[which(tep$Phase == "Phase III" & tep$GEO != "Malta")] ~tep$last_verified[which(tep$Phase == "Phase III" & tep$GEO != "Malta")])
  
  summary(lm3)
  my_plot <- ggplot(tep[which(tep$Phase == "Phase III" & tep$GEO != "Malta"),])+
    labs(title = "Phase III of EU ETS")+
    scale_x_log10()+
    scale_y_log10() +
    geom_point( aes(x = last_verified, y = Free, alpha = factor(year)))+
    geom_smooth(aes(x = last_verified, y = Free),method = "lm", se = FALSE, color = "black", size =0.5)+
    geom_line(aes(x = last_verified, y = Free, group = GEO, alpha = factor(year)))+
    scale_alpha_discrete(name = "Year")+
    annotate(
      "text",
      x = max(tep$last_verified),  # Set x to the maximum x-value
      y = min(tep$Free),  # Set y to the minimum y-value
      label = "Multiple R-squared:  0.9058
      Adjusted R-squared:  0.9053
      p-value: < 2.2e-16",
      vjust = 0,  # Adjust vertical alignment
      hjust = 1   # Adjust horizontal alignment
    )+
    xlab("Last year's verified emissions (log)") + 
    ylab("Free Allocation in t CO2 equivalent (log)") +
    theme(panel.background = element_rect(fill = rgb(220/255, 220/255, 220/255)),
          axis.title.x = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"),
          axis.title.y = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"),
          title = element_text(colour = rgb(183/255, 213/255, 73/255),face="bold"))
  print(my_plot)
  ggsave(filename = "./4σέλιδο/Phase_III.svg", plot = my_plot, device = "svg")
  
  
  
  ggplot(temp[which(temp$Phase == "Phase I"),])+
    labs(title = "Presentation 2", color = "Cluster")+
    geom_point( aes(x = Verified_emissions, y = Free, color = partition, alpha = year))+
    geom_smooth(aes(x = Verified_emissions, y = Free),method = "lm", se = FALSE, color = "black", size =0.5)+
    geom_line(aes(x = Verified_emissions, y = Free, color = partition, group = GEO, alpha = year))
  
}
