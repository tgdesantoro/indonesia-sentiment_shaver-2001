##analisis sentimen##
library(readr)
library(stringr)
library(tidyr)

data_clean <- read.csv("G:/My Drive/006_school/002_sentiment_supervise/data_clean_210925.csv")
View(data_clean)

emotion_id <- read.csv("C:/Users/Tri.Giyat/Documents/005_akademik/001_tesis/data/shaver_emotion_bahasa_ind.csv")
View(emotion_id)

# Pastikan kolom 'word' di unigram dan sentimentpost_id sama2 karakter
unigram$word <- as.character(unigram$word)
emotion_id$words <- as.character(emotion_id$words)

#tokenize unigram
unigram <- data_clean %>%
  mutate(line_id = row_number()) %>%                                      #Buat ID baris agar urutan tetap terjaga
  unnest_tokens(word,statement, token = "words", to_lower = TRUE) %>%     #tokenisasi = "words"/1 words| token = "ngrams", n = 2,3, ., etc
  group_by(state_id) %>%
  mutate(word_position = row_number()) %>%                                #Menyimpan posisi kata dalam dokumen
  ungroup()
View(unigram)

#SIAPKAN LEXICON EMOSI

#DETEKSI NEGASI
negation_terms <- c("tidak", "nggak", "ga", "gak", "tak", "bukan", "belum")


# format: emotion, basic_emotion, words (kata dipisahkan ;)
# contoh:
# happiness,positive,"bahagia;senang;gembira"
# anger,negative,"marah;benci;kesal"

unigram <- unigram %>%
  group_by(state_id) %>%
  mutate(
    negator_before = lag(word) %in% negation_terms,
    negator_two_before = lag(word, 2) %in% negation_terms,
    negated = ifelse(negator_before | negator_two_before, TRUE, FALSE)
  ) %>%
  ungroup()


lexicon_emotion_long <- emotion_id %>%
  separate_rows(words, sep = ";") %>%
  mutate(words = str_trim(str_to_lower(words)))  # pastikan lowercase

#JOIN TOKEN DENGAN LEXICON
unigram_emosi <- unigram %>%
  left_join(lexicon_emotion_long, by = c("word" = "words"))

#BALIKKAN VALENSI JIKA ADA NEGASI ===
unigram_emosi <- unigram_emosi %>%
  mutate(
    sentiment = case_when(
      negated & sentiment == "positif" ~ "negatif",
      negated & sentiment == "negatif" ~ "positif",
      TRUE ~ sentiment
    )
  )

#HITUNG JUMLAH KATA YANG COCOK PER STATE_ID DAN KATEGORI EMOSI
summary_emosi <- unigram_emosi %>%
  filter(!is.na(emotion)) %>%
  group_by(state_id, sentiment, emotion, basic.emotion) %>%
  summarise(
    emotion_word_count = n(),
    .groups = "drop"
  )

#BUAT VERSI RINGKAS PER STATE_ID (UNTUK GABUNG KE DATA ASLI) ===
summary_per_state <- summary_emosi %>%
  group_by(state_id) %>%
  summarise(
    total_emotion_words = sum(emotion_word_count),
    emotion_labels = paste(unique(emotion), collapse = "; "),
    basic_emotion_labels = paste(unique(basic.emotion), collapse = "; "),
    
    # Hitung total positif & negatif
    total_positive = sum(emotion_word_count[sentiment == "positif"], na.rm = TRUE),
    total_negative = sum(emotion_word_count[sentiment == "negatif"], na.rm = TRUE),
    
    # Nilai sentiment = positif - negatif
    sentiment_value = total_positive - total_negative
  )


# === 7. GABUNG KE DATA ASLI ===
data_analis_emotion <- data_clean %>%                 # <- Ganti dari data_analis → data_clean
  left_join(summary_per_state, by = "state_id")


# === 8. TAMBAHKAN KATEGORI EMOSI (NETRAL JIKA KOSONG) ===
data_analis <- data_analis_emotion %>%
  mutate(
    emotion_category = case_when(
      is.na(emotion_labels) ~ "neutral",
      sentiment_value > 0 ~ "positive",
      sentiment_value < 0 ~ "negative",
      sentiment_value == 0 ~ "neutral",
      TRUE ~ "neutral"
    )
  )

# === 9. (OPSIONAL) TAMPILKAN HASIL AKHIR ===
View(data_analis)
write.csv(data_analis,"C:/Users/Tri.Giyat/Documents/005_akademik/001_tesis/005_sentiment_shaver_rev.csv")

# === 10. (OPSIONAL) LIHAT RINGKASAN JUMLAH PER EMOSI ===
count_summary <- summary_emosi %>%
  group_by(emotion, basic_emotion) %>%
  summarise(
    total_occurrences = sum(emotion_word_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_occurrences))

count_summary

data_analis %>%
  select(state_id, statement, sentiment_value, emotion_category) %>%
  arrange(desc(sentiment_value)) %>%
  head(10)

sentiment_summary <- data_analis %>%
  group_by(emotion_category) %>%
  summarise(
    mean_sentiment = mean(sentiment_value, na.rm = TRUE),
    total_sentiment = sum(sentiment_value, na.rm = TRUE),
    n_statement = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_sentiment))

print(sentiment_summary)








