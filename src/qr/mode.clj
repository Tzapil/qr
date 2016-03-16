(ns qr.mode)

(def modes {
		:numeric {
			:prefix "0001"
			:length 10
		}
		:alphanumeric {
			:prefix "0010"
			:length 9
		}
		:byte {
			:prefix "0100"
			:length 8
		}
		:kanji {
			:prefix "1000"
			:length 8
		}
		:eci {
			:prefix "0111"
			:length 8
		}
	})