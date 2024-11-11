
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

compare_clustering_methods(normalize = "Mean", minNc = 3, maxNc = 5, year = 2005)

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
plot_correlation_matrix(2008)


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)    # Assuming read_data_2 uses readr functions
library(stringr)  # For string manipulation if needed

# Define the function
generate_scatterplot <- function(phase_number, attribute, use_log = FALSE, plot_path) {
  
  # Validate inputs
  valid_attributes <- c("Population", "GDPpc", "Total_energy_supply")
  if (!(attribute %in% valid_attributes)) {
    stop(paste("Invalid attribute. Choose from:", paste(valid_attributes, collapse = ", ")))
  }
  
  # Define phases based on phase_number
  phase_mapping <- list(
    "0" = 2004,
    "Phase I" = 2005:2007,
    "Phase II" = 2008:2012,
    "Phase III" = 2013:2020
  )
  
  selected_phase <- names(phase_mapping)[sapply(phase_mapping, function(x) phase_number %in% x)]
  if (is.null(selected_phase)) {
    stop("Invalid phase number.")
  }
  
  # Read and combine data
  years <- unlist(phase_mapping[selected_phase])
  temp <- lapply(years, function(year) {
    data <- read_data_2(year = year)
    data$Phase <- selected_phase
    data$year <- year
    return(data)
  }) %>% bind_rows()
  
  # Assign clusters using a more efficient approach
  cluster_df <- data.frame(GEO = unlist(clusters), partition = rep(c("First", "Second", "Third"), times = sapply(clusters, length)))
  temp <- temp %>% 
    left_join(cluster_df, by = "GEO")
  
  # Calculate additional variables if needed
  temp <- temp %>%
    mutate(
      actual_agri = Agriculture * GDPpc * Population,
      actual_ind = Industry * GDPpc * Population,
      actual_manu = Manufacturing * GDPpc * Population,
      tot_and_EI = Total_energy_supply * Energy_Intensity
    )
  
  # Calculate last_verified emissions
  temp <- temp %>%
    arrange(GEO, year) %>%
    group_by(GEO) %>%
    mutate(last_verified = lag(Verified_emissions, n = 1, order_by = year)) %>%
    ungroup()
  
  # Filter data for the selected phase
  filtered_data <- temp %>% filter(Phase == selected_phase)
  
  # Define cluster colors
  cluster_colors <- c("First" = "orange", "Second" = "green", "Third" = "blue")
  
  # Create the scatterplot
  plot <- ggplot(filtered_data, aes_string(x = attribute, y = "Free", color = "partition")) +
    geom_point(aes(alpha = year), size = 2) +
    # Connect points for the same country
    geom_line(aes(group = GEO, alpha = year), color = "black", size = 0.5) +
    # Add regression lines for each cluster
    geom_smooth(method = "lm", se = FALSE, aes(color = partition)) +
    scale_color_manual(values = cluster_colors) +
    # Adjust alpha based on year
    scale_alpha(range = c(0.5, 1), guide = FALSE) +
    # Apply log scale if requested
    { if (use_log) {
      list(
        scale_x_log10(),
        scale_y_log10()
      )
    } else {
      NULL
    }
    } +
    labs(
      title = paste("Free Allocation vs", attribute, "in Phase", phase_number),
      x = attribute,
      y = "Free Allocation",
      color = "Cluster"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = paste0("Phase_", phase_number, "_", attribute, ".svg"), plot = plot, path = plot_path, width = 8, height = 6)
  
  # Return the plot object
  return(plot)
}

# Example usage:
plot_dir <- "C:/Users/Kostas/Documents/GitHub/Diplomatiki_kwpap_step_1/Thesis/R_plots/04/"
plot1_path <- paste0(plot_dir, "test.png")
generate_scatterplot(phase_number = 3, attribute = "Population", use_log = TRUE, plot_path = plot1_path)

  