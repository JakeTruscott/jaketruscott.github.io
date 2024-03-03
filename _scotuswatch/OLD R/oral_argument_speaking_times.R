wrap_text <- function(x, width = 25) {
  gsub(sprintf('(?<=.{1,%d})\\s', width), '\n', x, perl = TRUE)
}

justice_image_df <- data.frame(speaker = names(justice_image_labels), label = justice_image_labels)

{
  October_sitting <- scotus_OT23 %>%
    left_join(justice_image_df, by = "speaker") %>%
    filter(speaker_type == 'Justice') %>%
    filter(sitting == 'October') %>%
    mutate(total_time = (text_stop - text_start)) %>%
    mutate(ideology = case_when(
      .default = 'Conservative',
      speaker %in% c('KAGAN', 'JACKSON', 'SOTOMAYOR') ~ 'Liberal'
    )) %>%
    group_by(case_name, speaker) %>%
    reframe(total_speaker_time = sum(total_time),
            ideology = ideology,
            label = label) %>%
    mutate(case_name = str_wrap(case_name, width = 40)) %>%
    ungroup() %>%
    unique() %>%
    mutate(total_speaker_time = total_speaker_time/60) %>%
    ggplot(aes(x = speaker, y = total_speaker_time)) +
    geom_col(aes(fill = ideology), colour = 'gray5') +
    scale_fill_manual(values = c('coral3', 'deepskyblue3')) +
    scale_x_discrete(labels = justice_image_labels) +
    facet_wrap(~case_name) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 10, face = 'italic', hjust = 0, vjust = -0.25),
      axis.title = element_text(size = 15),
      axis.text.x = ggtext::element_markdown(),
      axis.text = element_text(size = 14, colour = 'gray5'),
      panel.border = element_rect(linewidth = 1, colour = "gray5", fill = NA),
      legend.title.align=0.5,
      legend.title = element_blank(),
      legend.text = element_text(size = 15, colour = "gray5"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray95", colour = "gray5"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))  +
    guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
    labs(
      x = ' ',
      y = '\nTotal Speaking Time\n(Minutes)\n',
      title = 'October Sitting'
    )
} #October
{
  November_sitting <- scotus_OT23 %>%
    left_join(justice_image_df, by = "speaker") %>%
    filter(speaker_type == 'Justice') %>%
    filter(sitting == 'November') %>%
    mutate(total_time = (text_stop - text_start)) %>%
    mutate(ideology = case_when(
      .default = 'Conservative',
      speaker %in% c('KAGAN', 'JACKSON', 'SOTOMAYOR') ~ 'Liberal'
    )) %>%
    group_by(case_name, speaker) %>%
    reframe(total_speaker_time = sum(total_time),
            ideology = ideology,
            label = label) %>%
    mutate(case_name = str_wrap(case_name, width = 40)) %>%
    ungroup() %>%
    unique() %>%
    mutate(total_speaker_time = total_speaker_time/60) %>%
    ggplot(aes(x = speaker, y = total_speaker_time)) +
    geom_col(aes(fill = ideology), colour = 'gray5') +
    scale_fill_manual(values = c('coral3', 'deepskyblue3')) +
    scale_x_discrete(labels = justice_image_labels) +
    facet_wrap(~case_name) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 10, face = 'italic', hjust = 0, vjust = -0.25),
      axis.title = element_text(size = 15),
      axis.text.x = ggtext::element_markdown(),
      axis.text = element_text(size = 14, colour = 'gray5'),
      panel.border = element_rect(linewidth = 1, colour = "gray5", fill = NA),
      legend.title.align=0.5,
      legend.title = element_blank(),
      legend.text = element_text(size = 15, colour = "gray5"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray95", colour = "gray5"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))  +
    guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
    labs(
      x = ' ',
      y = '\nTotal Speaking Time\n(Minutes)\n',
      title = 'November Sitting'
    )
} #November
{
  December_sitting <- scotus_OT23 %>%
    left_join(justice_image_df, by = "speaker") %>%
    filter(speaker_type == 'Justice') %>%
    filter(sitting == 'December') %>%
    mutate(total_time = (text_stop - text_start)) %>%
    mutate(ideology = case_when(
      .default = 'Conservative',
      speaker %in% c('KAGAN', 'JACKSON', 'SOTOMAYOR') ~ 'Liberal'
    )) %>%
    group_by(case_name, speaker) %>%
    reframe(total_speaker_time = sum(total_time),
            ideology = ideology,
            label = label) %>%
    mutate(case_name = str_wrap(case_name, width = 40)) %>%
    ungroup() %>%
    unique() %>%
    mutate(total_speaker_time = total_speaker_time/60) %>%
    ggplot(aes(x = speaker, y = total_speaker_time)) +
    geom_col(aes(fill = ideology), colour = 'gray5') +
    scale_fill_manual(values = c('coral3', 'deepskyblue3')) +
    scale_x_discrete(labels = justice_image_labels) +
    facet_wrap(~case_name) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 10, face = 'italic', hjust = 0, vjust = -0.25),
      axis.title = element_text(size = 15),
      axis.text.x = ggtext::element_markdown(),
      axis.text = element_text(size = 14, colour = 'gray5'),
      panel.border = element_rect(linewidth = 1, colour = "gray5", fill = NA),
      legend.title.align=0.5,
      legend.title = element_blank(),
      legend.text = element_text(size = 15, colour = "gray5"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray95", colour = "gray5"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))  +
    guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
    labs(
      x = ' ',
      y = '\nTotal Speaking Time\n(Minutes)\n',
      title = 'December Sitting'
    )
} #December
{
  January_sitting <- scotus_OT23 %>%
    left_join(justice_image_df, by = "speaker") %>%
    filter(speaker_type == 'Justice') %>%
    filter(sitting == 'January') %>%
    mutate(total_time = (text_stop - text_start)) %>%
    mutate(ideology = case_when(
      .default = 'Conservative',
      speaker %in% c('KAGAN', 'JACKSON', 'SOTOMAYOR') ~ 'Liberal'
    )) %>%
    group_by(case_name, speaker) %>%
    reframe(total_speaker_time = sum(total_time),
            ideology = ideology,
            label = label) %>%
    mutate(case_name = str_wrap(case_name, width = 40)) %>%
    ungroup() %>%
    unique() %>%
    mutate(total_speaker_time = total_speaker_time/60) %>%
    ggplot(aes(x = speaker, y = total_speaker_time)) +
    geom_col(aes(fill = ideology), colour = 'gray5') +
    scale_fill_manual(values = c('coral3', 'deepskyblue3')) +
    scale_x_discrete(labels = justice_image_labels) +
    facet_wrap(~case_name) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 10, face = 'italic', hjust = 0, vjust = -0.25),
      axis.title = element_text(size = 15),
      axis.text.x = ggtext::element_markdown(),
      axis.text = element_text(size = 14, colour = 'gray5'),
      panel.border = element_rect(linewidth = 1, colour = "gray5", fill = NA),
      legend.title.align=0.5,
      legend.title = element_blank(),
      legend.text = element_text(size = 15, colour = "gray5"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray95", colour = "gray5"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))  +
    guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
    labs(
      x = ' ',
      y = '\nTotal Speaking Time\n(Minutes)\n',
      title = 'January Sitting'
    )
} #January
{
  February_sitting <- scotus_OT23 %>%
    left_join(justice_image_df, by = "speaker") %>%
    filter(speaker_type == 'Justice') %>%
    filter(sitting == 'February') %>%
    mutate(total_time = (text_stop - text_start)) %>%
    mutate(ideology = case_when(
      .default = 'Conservative',
      speaker %in% c('KAGAN', 'JACKSON', 'SOTOMAYOR') ~ 'Liberal'
    )) %>%
    group_by(case_name, speaker) %>%
    reframe(total_speaker_time = sum(total_time),
            ideology = ideology,
            label = label) %>%
    mutate(case_name = str_wrap(case_name, width = 40)) %>%
    ungroup() %>%
    unique() %>%
    mutate(total_speaker_time = total_speaker_time/60) %>%
    ggplot(aes(x = speaker, y = total_speaker_time)) +
    geom_col(aes(fill = ideology), colour = 'gray5') +
    scale_fill_manual(values = c('coral3', 'deepskyblue3')) +
    scale_x_discrete(labels = justice_image_labels) +
    facet_wrap(~case_name) +
    geom_hline(yintercept = 0, colour = 'gray5') +
    theme_minimal() +
    theme(
      plot.caption = element_text(size = 10, face = 'italic', hjust = 0, vjust = -0.25),
      axis.title = element_text(size = 15),
      axis.text.x = ggtext::element_markdown(),
      axis.text = element_text(size = 14, colour = 'gray5'),
      panel.border = element_rect(linewidth = 1, colour = "gray5", fill = NA),
      legend.title.align=0.5,
      legend.title = element_blank(),
      legend.text = element_text(size = 15, colour = "gray5"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray95", colour = "gray5"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5))  +
    guides(size = guide_legend(title.position="top", title.hjust = 0.5)) +
    labs(
      x = ' ',
      y = '\nTotal Speaking Time\n(Minutes)\n',
      title = 'February Sitting'
    )
} #February

October_sitting
November_sitting
December_sitting
January_sitting
February_sitting
